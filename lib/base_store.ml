open Result

let src_log = Logs.Src.create "pih-store"
module Log = (val Logs.src_log src_log : Logs.LOG)

let (>>=) = Lwt.bind
let return = Lwt.return

module Context = struct let v () = return None end
module Store_Maker = Irmin_mirage.Irmin_git.Memory(Context)(Inflator)
module Store = Store_Maker(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)

type t = string -> Store.t
type key = Irmin.Contents.String.Path.t
type value = Irmin.Contents.String.t
type commit_id = Irmin.Hash.SHA1.t

let init ~owner =
  let task s = Irmin.Task.create ~date:0L ~owner s in
  let config = Irmin_mem.config () in
  Store.Repo.create config >>= fun repo ->
  Store.master task repo

module Pair = Tc.Pair(Store.Hash)(Store.Private.Slice)

let dump ?min s =
  let s = s "export" in
  Store.head s >>= function
  | None -> return (Error (Invalid_argument "dump: no head"))
  | Some head ->
     match min with
     | Some min when List.length min <> 0
                     && Irmin.Hash.SHA1.equal head (List.hd min) ->
        return (Ok None)
     | _ ->
        Store.Repo.export ?min ~max:[head] (Store.repo s) >>= fun slice ->
        let p = (head, slice) in
        let buf = Cstruct.create (Pair.size_of p) in
        let _ = Pair.write p buf in
        let dump = Cstruct.to_string buf in
        return (Ok (Some (head, dump)))


let inspect_slice slice =
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
  Slice.iter_contents slice log_content


let import s dump =
  let s = s "import" in
  let buf = Mstruct.of_string dump in
  let (head, slice) = Pair.read buf in
  Log.debug (fun f -> f "import head: %s" (Store.Hash.to_hum head));
  inspect_slice slice >>= fun () ->
  Store.Repo.import (Store.repo s) slice >>= function
  | `Error ->
     Log.err (fun f -> f "import error");
     return (Error (Failure "import: import failure"))
  | `Ok ->
     Store.fast_forward_head s head >>= function
     | false -> return (Error (Failure "import: ff failure"))
     | true -> return (Ok head)

let read s key =
  let s = s  "read" in
  Store.read s key >>= function
  | None -> return (Error Not_found)
  | Some v -> return (Ok v)

let update s ~check key c =
  if not (check c)
  then return (Error (Invalid_argument c))
  else
    let s = s "update" in
    Store.update s key c
    >>= fun () -> return (Ok ())

let create s ~check key c =
  if not (check c)
  then return (Error (Invalid_argument c))
  else
    let s = s "create" in
    Store.update s key c
    >>= fun () -> return (Ok ())

let remove s key =
  let s = s "remove" in
  Store.remove s key
  >>= fun () -> return (Ok ())

let list s ?parent () =
  let key = match parent with
    | None -> [] | Some lst -> lst in
  let s = s "list" in
  Store.list s key
  >>= fun keys -> return (Ok keys)
