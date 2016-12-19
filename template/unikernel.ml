open V1
open V1_LWT
open Lwt
open Result

let log_src = Logs.Src.create "pih-store"
module Log = (val Logs.src_log log_src : Logs.LOG)
module Client = Cohttp_mirage.Client

module Store = struct

  module S = Data_store


  let with_ok_unit t = t >>= function
    | Ok () -> return (Ok "")
    | Error _ as e -> return e

  let with_ok_list t = t >>= function
    | Ok lst ->
       let r = Ezjsonm.(list string lst |> to_string) in
       return (Ok r)
    | Error _ as e -> return e

  let dispatch store ?src body steps =
    match steps with
    | "create" :: id ->
       with_ok_unit (S.create store ?src id body)
    | "read" :: id ->
       S.read store ?src id
    | "update" :: id ->
       with_ok_unit (S.update store ?src id body)
    | "delete" :: id ->
       with_ok_unit (S.remove store ?src id)
    | "list" :: parent ->
       with_ok_list (S.list store ~parent ?src ())
    | _ -> return (Error Not_found)


  let init ?remote ~time () =
    let owner = "pih-store" in
    let backend = match remote with
      | Some remote_conf -> `Http (remote_conf, owner)
      | None -> `Memory owner in
    S.make ~backend ~time ()

end


module Dispatcher
    (Http: Cohttp_lwt.Server) = struct

  let headers = Cohttp.Header.init_with
    "Strict-Transport-Security" "max-age=31536000"
  let empty_body = Cohttp_lwt_body.empty


  let dispatcher store ?src uri req body =
    let p = Uri.path uri in
    let steps = Astring.String.cuts ~empty:false ~sep:"/" p in
    Cohttp_lwt_body.to_string body >>= fun v ->
    Store.dispatch store ?src v steps
    >>= function
    | Ok "" -> Http.respond ~headers ~status:`OK ~body:empty_body ()
    | Ok json ->
       let headers = Cohttp.Header.add headers
         "content-type" "application/json" in
       let body = Cohttp_lwt_body.of_string json in
       Http.respond ~headers ~status:`OK ~body ()
    | Error e ->
       let body = Printexc.to_string e |> Cohttp_lwt_body.of_string in
       Http.respond ~headers ~status:`Not_found ~body ()


  let serve dispatch =
    let callback (_, cid) request body =
      let src = None in
      let uri = Cohttp.Request.uri request in
      let cid = Cohttp.Connection.to_string cid in
      Log.info (fun f -> f  "[%s] serving %s." cid (Uri.to_string uri));
      dispatch ?src uri request body
    in
    let conn_closed (_,cid) =
      let cid = Cohttp.Connection.to_string cid in
      Log.info (fun f -> f "[%s] closing" cid);
    in
    Http.make ~conn_closed ~callback ()
end


module Main
     (Http: Cohttp_lwt.Server)
     (Resolver: Resolver_lwt.S)
     (Conduit: Conduit_mirage.S)
     (Clock: V1.CLOCK) = struct

  module Logs_reporter = Mirage_logs.Make(Clock)
  module D = Dispatcher(Http)


  let async_hook exn =
    Log.debug (fun f -> f "async hook: %s" (Printexc.to_string exn))


  let start http resolver conduit clock =
    Lwt.async_exception_hook := async_hook;

    let persist_remote = Key_gen.persist_remote () in
    let persist_host = Key_gen.persist_host () |> Ipaddr.V4.to_string in
    let persist_port = Key_gen.persist_port () in
    let persist_uri = Uri.make ~scheme:"http" ~host:persist_host ~port:persist_port () in

    let time () = Clock.time () |> string_of_float in

    (if persist_remote then Store.init ~remote:(resolver, conduit, persist_uri) ~time ()
     else Store.init ~time ())
    >>= fun s ->

    http (`TCP 8080) @@ D.serve (D.dispatcher s);
end
