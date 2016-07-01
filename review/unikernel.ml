open V1
open V1_LWT
open Lwt
open Result

let src = Logs.Src.create "ucn.review"
module Log = (val Logs.src_log src : Logs.LOG)

module Store = struct

  module Context = struct let v () = return_none end
  module Mirage_git_memory = Irmin_mirage.Irmin_git.Memory(Context)(Git.Inflate.None)
  module S = Mirage_git_memory(Irmin.Contents.Json)(Irmin.Ref.String)(Irmin.Hash.SHA1)

  let data_path = ["data"]
  let log_path = ["logs"]

  let check_fields = function
    | `O _ as d ->
       let l = Ezjsonm.get_dict d in
       (List.mem_assoc "id" l
        && List.mem_assoc "title" l
        && List.mem_assoc "rating" l
        && List.mem_assoc "comment" l)
       |> ignore;
       return_unit
    | _ as v ->
       fail_invalid_arg "not json object"


  let read s id =
    let t = "read review " ^ id in
    Log.app (fun msgf -> msgf "%s" t);
    let s = s t in
    let p = data_path @ [id] in
    S.read s p >>= function
    | None ->
       Log.info (fun f -> f "not found review %s" id);
       return (Error Not_found)
    | Some v ->
       let v = Ezjsonm.to_string v in
       Log.info (fun f -> f "read_review respond: %s" v);
       return (Ok v)


  let update s id body =
    let t = "create/update new review " ^ id in
    Log.app (fun f -> f "%s" t);
    let s = s t in
    let p = data_path @ [id] in
    catch (fun () ->
      Cohttp_lwt_body.to_string body >>= fun body ->
      let v = Ezjsonm.from_string body in
      check_fields v >>= fun () ->
      S.update s p v >>= fun () ->
      Log.info (fun f -> f "review created/updated");
      return (Ok "")
      ) (fun e ->
      Log.app (fun f -> f "exn: %s" (Printexc.to_string e));
      return (Error e))


  let remove s id =
    let t = "remove review " ^ id in
    Log.app (fun f -> f "%s" t);
    let s = s t in
    let p = data_path @ [id] in
    S.remove s p >>= fun () ->
    Log.info (fun f -> f "remove review %s" id);
    return (Ok "")


  let meta_of_review = function
    | `O obj ->
       let title = List.assoc "title" obj in
       let meta = ["source", `String "review"; "title", title] in
       return Ezjsonm.(dict meta |> to_string)
    | _ -> Lwt.fail_with "never"


  let get_meta s id =
    let t = "get metadata of a review " ^ id in
    Log.app (fun f -> f "%s" t);
    let s = s t in
    let p = data_path @ [id] in
    S.read s p >>= function
    | None ->
       Log.info (fun f -> f "not found review %s for metadata" id);
       return (Error Not_found)
    | Some v ->
       meta_of_review v >>= fun meta ->
       Log.info (fun msgf -> msgf "respond metadata %s" meta);
       return (Ok meta)


  let list s =
    let t = "list id of reviewed movies" in
    Log.app (fun f -> f "%s" t);
    let s = s t in
    let p = data_path in
    S.list s p >>= fun keys ->
    let id_lst = List.map (fun key -> key |> List.rev |> List.hd) keys in
    Log.info (fun f -> f "list respond: [%s]" (String.concat " " id_lst));
    let arr = Ezjsonm.(list string id_lst) in
    return (Ok (Ezjsonm.to_string arr))


  let dispatch store body steps =
    match steps with
    | ["create"; id] -> update store id body
    | ["read"; id] -> read store id
    | ["update"; id] -> update store id body
    | ["delete"; id] -> remove store id
    | ["meta"; id] -> get_meta store id
    | ["list"] -> list store
    | _ -> return (Error Not_found)


  let init () =
    let task s = Irmin.Task.create ~date:0L ~owner:"ucn.review" s in
    let config = Irmin_mem.config () in
    S.Repo.create config >>= fun repo ->
    S.master task repo
end


module Dispatch
    (Http: Cohttp_lwt.Server) = struct

  let headers = Cohttp.Header.init_with
    "Strict-Transport-Security" "max-age=31536000"
  let empty_body = Cohttp_lwt_body.empty


  let review_dispatcher store uri req body =
    let p = Uri.path uri in
    let steps = Astring.String.cuts ~empty:false ~sep:"/" p in
    Store.dispatch store body steps
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


  let redirect uri _req _body =
    let new_uri = Uri.with_scheme uri (Some "https") in
    let new_uri = Uri.with_port new_uri (Some 4433) in
    let headers =
      Cohttp.Header.add headers "location" (Uri.to_string new_uri)
    in
    Http.respond ~headers ~status:`Moved_permanently ~body:`Empty ()


  let serve dispatch =
    let callback (_, cid) request body =
      let uri = Cohttp.Request.uri request in
      let cid = Cohttp.Connection.to_string cid in
      Log.info (fun f -> f  "[%s] serving %s." cid (Uri.to_string uri));
      dispatch uri request body
    in
    let conn_closed (_,cid) =
      let cid = Cohttp.Connection.to_string cid in
      Log.info (fun f -> f "[%s] closing" cid);
    in
    Http.make ~conn_closed ~callback ()
end


module Main
     (Http: Cohttp_lwt.Server)
     (Keys: KV_RO)
     (Clock: V1.CLOCK) = struct

  module X509 = Tls_mirage.X509(Keys)(Clock)

  module D = Dispatch(Http)

  module Logs_reporter = Mirage_logs.Make(Clock)

  let tls_init kv =
    X509.certificate kv `Default >>= fun cert ->
    let conf = Tls.Config.server ~certificates:(`Single cert) () in
    Lwt.return conf

  let start http keys _clock =
    Logs.(set_level (Some Info));
    Logs_reporter.(create () |> run) @@ fun () ->

    tls_init keys >>= fun cfg ->
    let tcp = `TCP 4433 in
    let tls = `TLS (cfg, tcp) in

    Store.init () >>= fun store ->

    Lwt.join [
      http tls @@ D.serve (D.review_dispatcher store);
      http (`TCP 8081) @@ D.serve D.redirect
    ]
end
