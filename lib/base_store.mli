module type STRING_IRMIN = Irmin.S with
    type commit_id = Irmin.Hash.SHA1.t and
    type key = Irmin.Contents.String.Path.t and
    type value = Irmin.Contents.String.t


type t = S: (module STRING_IRMIN with type t = 'a) * (string -> 'a) -> t

val init : (module STRING_IRMIN with type t = 'a) -> (string -> 'a) -> t

val mem_backend : owner:string -> t Lwt.t

val http_backend : Resolver_lwt.t -> Conduit_mirage.conduit -> Uri.t -> owner:string -> t Lwt.t

val dump :
  ?min:Irmin.Hash.SHA1.t list ->
  t -> ((Irmin.Hash.SHA1.t * string) option, exn) Result.result Lwt.t

val import : t -> string -> (Irmin.Hash.SHA1.t, exn) Result.result Lwt.t

val read :
  t ->
  Irmin.Contents.String.Path.t ->
  (Irmin.Contents.String.t, exn) Result.result Lwt.t

val update :
  t ->
  check:(Irmin.Contents.String.t -> bool) ->
  Irmin.Contents.String.Path.t ->
  Irmin.Contents.String.t -> (unit, exn) Result.result Lwt.t

val create :
  t ->
  check:(Irmin.Contents.String.t -> bool) ->
  Irmin.Contents.String.Path.t ->
  Irmin.Contents.String.t -> (unit, exn) Result.result Lwt.t

val remove :
  t -> Irmin.Contents.String.Path.t -> (unit, 'a) Result.result Lwt.t

val remove_rec :
  t -> Irmin.Contents.String.Path.t -> (unit, 'a) Result.result Lwt.t

val list :
  t ->
  ?parent:Irmin.Contents.String.Path.t ->
  unit -> (Irmin.Contents.String.Path.t list, 'a) Result.result Lwt.t
