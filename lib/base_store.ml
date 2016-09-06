open Result

let src_log = Logs.Src.create "pih-store"
module Log = (val Logs.src_log src_log : Logs.LOG)

let (>>=) = Lwt.bind
let return = Lwt.return

(*
type key = Irmin.Contents.String.Path.t
type value = Irmin.Contents.String.t
type commit_id = Irmin.Hash.SHA1.t

module Context = struct let v () = return None end
module Store_Maker = Irmin_mirage.Irmin_git.Memory(Context)(Inflator)
module Store = Store_Maker(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1) *)

(*
let init ~owner =
  let task s = Irmin.Task.create ~date:0L ~owner s in
  let config = Irmin_mem.config () in
  Store.Repo.create config >>= fun repo ->
  Store.master task repo *)


(*
let inspect_slice (module Store : Irmin.S) s slice =
  let open Store.Private in
  let log_kv n (k, v) fn_k fn_v =
    Log.debug (fun f -> f "%s [%s]:[%s]" n (fn_k k) (fn_v v));
    Lwt.return_unit in
  let value_to_string v =
    Ezjsonm.wrap v |> Ezjsonm.to_string in
  let log_content c =
    let fn_v v = v |> Contents.Val.to_json |> value_to_string in
    log_kv "content" c Contents.Key.to_hum fn_v in
  let log_node n =
    let fn_v v = v |> Node.Val.to_json |> value_to_string in
    log_kv "node" n Node.Key.to_hum fn_v in
  let log_commit c =
    let fn_v v = v |> Commit.Val.to_json |> value_to_string in
    log_kv "commit" c Commit.Key.to_hum fn_v in
  Slice.iter_commits slice log_commit >>= fun () ->
  Slice.iter_nodes slice log_node >>= fun () ->
  Slice.iter_contents slice log_content *)

module type STRING_IRMIN = Irmin.S with
    type commit_id = Irmin.Hash.SHA1.t and
    type key = Irmin.Contents.String.Path.t and
    type value = Irmin.Contents.String.t


type t = S: (module STRING_IRMIN with type t = 'a) * (string -> 'a) -> t


let init m t = S (m, t)


let mem_backend ~owner =
  let module Context = struct let v () = return None end in
  let module Store_Maker = Irmin_mirage.Irmin_git.Memory(Context)(Inflator) in
  let module Store = (Store_Maker(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1) : STRING_IRMIN) in

  let task s = Irmin.Task.create ~date:0L ~owner s in
  let config = Irmin_mem.config () in
  Store.Repo.create config >>= fun repo ->
  Store.master task repo >>= fun s ->
  return @@ S ((module Store), s)


let http_backend resolver conduit uri ~owner =
  let module Client = Cohttp_mirage.Client in
  let module Context_Client = (struct
      include Client
      let ctx = Client.ctx resolver conduit
      let get ?(ctx=ctx) = Client.get ~ctx
      let post ?(ctx=ctx) = Client.post ~ctx
      let delete ?(ctx=ctx) = Client.delete ~ctx
    end : Cohttp_lwt.Client) in

  let module Store_Maker = Irmin_http.Make(Context_Client)(Irmin.Metadata.None) in
  let module Store = Store_Maker(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1) in
  let task s = Irmin.Task.create ~date:0L ~owner s in
  let config = Irmin_http.config uri in

  Store.Repo.create config >>= fun repo ->
  Store.master task repo >>= fun s ->
  return @@ S ((module Store), s)


let dump ?min = function
  | S ((module Store), s) ->
     let s = s "export" in
     Store.head s >>= function
     | None -> return (Error (Invalid_argument "dump: no head"))
     | Some head ->
        match min with
        | Some min when List.length min <> 0 && Irmin.Hash.SHA1.equal head (List.hd min) ->
           return (Ok None)
        | _ ->
           let module Pair = Tc.Pair(Store.Hash)(Store.Private.Slice) in
           Store.Repo.export ?min ~max:[head] (Store.repo s) >>= fun slice ->
           let p = (head, slice) in
           let buf = Cstruct.create (Pair.size_of p) in
           let _ = Pair.write p buf in
           let dump = Cstruct.to_string buf in
           return (Ok (Some (head, dump)))


let import s dump = match s with
  | S ((module Store), s) ->
     let module Pair = Tc.Pair(Store.Hash)(Store.Private.Slice) in
     let s = s "import" in
     let buf = Mstruct.of_string dump in
     let (head, slice) = Pair.read buf in
     Log.debug (fun f -> f "import head: %s" (Store.Hash.to_hum head));

     let open Store.Private in
     let log_kv n (k, v) fn_k fn_v =
       Log.debug (fun f -> f "%s [%s]:[%s]" n (fn_k k) (fn_v v));
       Lwt.return_unit in
     let value_to_string v =
       Ezjsonm.wrap v |> Ezjsonm.to_string in
     let log_content c =
       let fn_v v = v |> Contents.Val.to_json |> value_to_string in
       log_kv "content" c Contents.Key.to_hum fn_v in
     let log_node n =
       let fn_v v = v |> Node.Val.to_json |> value_to_string in
       log_kv "node" n Node.Key.to_hum fn_v in
     let log_commit c =
       let fn_v v = v |> Commit.Val.to_json |> value_to_string in
       log_kv "commit" c Commit.Key.to_hum fn_v in
     Slice.iter_commits slice log_commit >>= fun () ->
     Slice.iter_nodes slice log_node >>= fun () ->
     Slice.iter_contents slice log_content >>= fun () ->

     Store.Repo.import (Store.repo s) slice >>= function
     | `Error ->
        Log.err (fun f -> f "import error");
        return (Error (Failure "import: import failure"))
     | `Ok ->
        Store.fast_forward_head s head >>= function
        | false -> return (Error (Failure "import: ff failure"))
        | true -> return (Ok head)


let read s key log = match s with
  | S ((module Store), s) ->
     let s = s log in
     Store.read s key >>= function
     | None -> return (Error Not_found)
     | Some v -> return (Ok v)


let update s ~check key c log = match s with
  | S ((module Store), s) ->
     if not (check c)
     then return (Error (Invalid_argument c))
     else
       let s = s log in
       Store.update s key c
       >>= fun () -> return (Ok ())


let create s ~check key c log = match s with
  | S ((module Store), s) ->
     if not (check c)
     then return (Error (Invalid_argument c))
     else
       let s = s log in
       Store.update s key c
       >>= fun () -> return (Ok ())


let remove s key log = match s with
  | S ((module Store), s) ->
     let s = s log in
     Store.remove s key
     >>= fun () -> return (Ok ())


let remove_rec s key log = match s with
  | S ((module Store), s) ->
     let s = s log in
     Store.remove_rec s key
     >>= fun () -> return (Ok ())


let list s ?parent log () = match s with
  | S ((module Store), s) ->
     let key = match parent with
       | None -> [] | Some lst -> lst in
     let s = s log in
     Store.list s key
     >>= fun keys -> return (Ok keys)
