open Result

let (>>=) = Lwt.bind
let return = Lwt.return

module Context = struct let v () = return None end
module Store_Maker = Irmin_mirage.Irmin_git.Memory(Context)(Inflator)
module Store = Store_Maker(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)

type t = string -> Store.t
type key = Irmin.Contents.String.Path.t
type value = Irmin.Contents.String.t

let init ~owner =
  let task s = Irmin.Task.create ~date:0L ~owner s in
  let config = Irmin_mem.config () in
  Store.Repo.create config >>= fun repo ->
  Store.master task repo

module Pair = Tc.Pair(Store.Hash)(Store.Private.Slice)

let dump s =
  let s = s "export" in
  Store.head s >>= function
  | None -> return (Error (Invalid_argument "dump: no head"))
  | Some head ->
     Store.Repo.export ~max:[head] (Store.repo s) >>= fun slice ->
     let p = (head, slice) in
     let buf = Cstruct.create (Pair.size_of p) in
     let _ = Pair.write p buf in
     let dump = Cstruct.to_string buf in
     return (Ok dump)

let import s dump =
  let s = s "import" in
  let buf = Mstruct.of_string dump in
  let (head, slice) = Pair.read buf in
  Store.Repo.import (Store.repo s) slice >>= function
  | `Error -> return (Error (Failure "import: import failure"))
  | `Ok ->
     Store.fast_forward_head s head >>= function
     | false -> return (Error (Failure "import: ff failure"))
     | true -> return (Ok ())

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
