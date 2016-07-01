open Result

module Make(C : V1.CLOCK): sig
  type t
  type key = Base_store.key
  type value = Base_store.value
  type src = string * int
  type id = string

  val make : owner:string -> ?check:(value -> bool) -> unit -> t Lwt.t

  val read : ?src:src -> t -> id -> (value, exn) result Lwt.t

  val update :?src:src -> t -> id -> value -> (unit, exn) result Lwt.t

  val create : ?src:src -> t -> id -> value -> (unit, exn) result Lwt.t

  val remove : ?src:src -> t -> id -> (unit, exn) result Lwt.t

  val list : ?src:src -> t -> (key list, exn) result Lwt.t

  val get_meta : ?src:src -> t -> id -> (value -> 'a) -> ('a, exn) result Lwt.t

  val list_logs : ?src:src -> t -> (string list, exn) result Lwt.t

  val read_log : ?src:src -> t -> id -> (string, exn) result Lwt.t
end
