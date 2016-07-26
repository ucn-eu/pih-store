open Result

type t
type value = string
type src = string * int
type key = string list
type id = string

val make :
  owner:string ->
  time:(unit -> string) ->
  ?check:(value -> bool) ->
  ?dump:string -> unit -> t Lwt.t

val export: t -> (string, exn) result Lwt.t

val read : t -> ?src:src -> key -> (value, exn) result Lwt.t

val update : t -> ?src:src -> key -> value -> (unit, exn) result Lwt.t

val create : t -> ?src:src -> key -> value -> (unit, exn) result Lwt.t

val remove : t -> ?src:src -> key -> (unit, exn) result Lwt.t

val list : t -> ?src:src -> ?parent:key -> unit -> (id list, exn) result Lwt.t

val get_meta : t -> ?src:src -> key -> (value -> 'a) -> ('a, exn) result Lwt.t

val list_logs : t -> ?src:src -> unit -> (string list, exn) result Lwt.t

val read_log : t -> ?src:src -> id -> (string, exn) result Lwt.t
