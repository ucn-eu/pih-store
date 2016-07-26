open Result

type t
type key = string list
type value = string

val init : owner:string -> t Lwt.t

val dump: t -> (string, exn) result Lwt.t

val import: t -> string -> (unit, exn) result Lwt.t

val read : t -> key -> (value, exn) result Lwt.t

val update : t -> check:(value -> bool) -> key -> value -> (unit, exn) result Lwt.t

val create : t -> check:(value -> bool) -> key -> value -> (unit, exn) result Lwt.t

val remove : t -> key -> (unit, 'a) result Lwt.t

val list : t -> ?parent:key -> unit -> (key list, 'a) result Lwt.t
