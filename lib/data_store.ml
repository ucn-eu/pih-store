open Result

module B = Base_store

let (>>=) = Lwt.bind
let return = Lwt.return

type t = {
    store: B.t;
    time : unit -> string;
    check: B.value -> bool;
}
type value = B.value
type key = string list
type id = string
type src = string * int

let make ~owner ~time ?check ?dump () =
  B.init ~owner >>= fun store ->
  let check = match check with
    | Some c -> c | None -> fun _ -> true in
  match dump with
  | None -> return {store; time; check}
  | Some d ->
     B.import store d >>= function
     | Error _ (*log this*)
     | Ok () -> Lwt.return {store; time; check}

let export {store; _} = B.dump store

let data_root = "data"
let log_root = "log"

let with_log s t ?src a k =
  let src = match src with
    | None -> "unknown"
    | Some (ip, port) -> Printf.sprintf "%s:%d" ip port in
  let time = t () in
  k () >>= fun r ->
  let re =
    match r with
    | Ok _ -> "succeed"
    | Error e-> "fail: " ^ (Printexc.to_string e) in
  let log =
    Printf.sprintf "[%s] %s %s %s" time src a re in
  B.create s ~check:(fun _ -> true) (log_root :: [time]) log
  >>= fun _ -> Lwt.return r

let read {store; time; _} ?src key =
  let path = data_root :: key in
  let fn () = B.read store path in
  let action = "read " ^ (String.concat "/" path) in
  with_log store time ?src action fn

let update {store; time; check} ?src key v =
  let path = data_root :: key in
  let fn () = B.update store ~check path v in
  let action = Printf.sprintf "update %s" (String.concat "/" path) in
  with_log store time ?src action fn

let create {store; time; check} ?src key v =
  let path = data_root :: key in
  let fn () = B.create store ~check (data_root :: path) v in
  let action = Printf.sprintf "create %s" (String.concat "/" path) in
  with_log store time ?src action fn

let remove {store; time; _} ?src key =
  let path = data_root :: key in
  let fn () = B.remove store (data_root :: path) in
  let action = "remove " ^ (String.concat "/" path) in
  with_log store time ?src action fn

let list {store; time; _} ?src ?parent () =
  let parent = match parent with
    | None -> [data_root]
    | Some p -> data_root :: p  in
  let fn () =
    B.list store ~parent () >>= function
    | Ok lst ->
       let id_of_k k = k |> List.rev |> List.hd in
       let id_lst = List.map id_of_k lst in
       return (Ok id_lst)
    | Error _ as e -> return e
  in
  with_log store time ?src "list data" fn

let get_meta {store;time;  _} ?src key to_meta =
  let path = data_root :: key in
  let fn () =
    B.read store (data_root :: path) >>= function
    | Ok v -> return (Ok (to_meta v))
    | Error _ as e -> return e in
  let action = "get metadata of " ^ (String.concat "/" path) in
  with_log store time ?src action fn

let compare x y =
  let x = Astring.String.cuts ~empty:false ~sep:":" x in
  let y = Astring.String.cuts ~empty:false ~sep:":" y in
  let rec aux = function
    | [], [] -> 0
    | hx :: tx, hy :: ty ->
       let c = Pervasives.compare hx hy in
       if c = 0 then aux (tx, ty)
       else c
    | _ -> assert false in
  aux (x, y)

let list_logs {store; time; _} ?src () =
  let parent = [log_root] in
  let fn () =
    B.list store ~parent () >>= function
    | Ok lst ->
       let id_of_key = fun key -> key |> List.rev |> List.hd in
       let id_lst = List.map id_of_key lst in
       let sorted = List.sort compare id_lst in
       return (Ok sorted)
    | Error _ as e -> return e
  in
  with_log store time ?src "list logs" fn

let read_log {store; time; _} ?src id =
  let fn () = B.read store (log_root :: [id]) in
  let action = "read log " ^ id in
  with_log store time ?src action fn
