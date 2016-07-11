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
