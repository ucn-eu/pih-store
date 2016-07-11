open Result

type t
type value = string
type src = string * int
type id = string

val make :
  owner:string ->
  time:(unit -> string) ->
  ?check:(value -> bool) -> unit -> t Lwt.t

val read : t -> ?src:src -> id -> (value, exn) result Lwt.t

val update : t -> ?src:src -> id -> value -> (unit, exn) result Lwt.t

val create : t -> ?src:src -> id -> value -> (unit, exn) result Lwt.t

val remove : t -> ?src:src -> id -> (unit, exn) result Lwt.t

val list : t -> ?src:src -> unit -> (id list, exn) result Lwt.t

val get_meta : t -> ?src:src -> id -> (value -> 'a) -> ('a, exn) result Lwt.t

val list_logs : t -> ?src:src -> unit -> (string list, exn) result Lwt.t

val read_log : t -> ?src:src -> id -> (string, exn) result Lwt.t
