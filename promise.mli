type ('a, 'b) t

type 'a resolve = 'a -> unit

type 'a reject = 'a -> unit

val is_supported : unit -> bool

val make : ('a resolve -> 'b reject -> unit) -> ('a, 'b) t

val resolve : 'a -> ('a, 'b) t

val reject : 'b -> ('a, 'b) t

val then_bind :
  on_fulfilled:('a -> ('c ,'b) t) ->
  ?on_rejected:('b -> ('c, 'b) t) ->
  ('a, 'b) t ->
  ('c, 'b) t

val then_map :
  on_fulfilled:('a -> 'c) ->
  ?on_rejected:('b -> 'c) ->
  ('a, 'b) t ->
  ('c, 'b) t

val catch_bind :
  on_rejected:('b -> ('a, 'b) t) ->
  ('a, 'b) t ->
  ('a, 'b) t

val catch_map :
  on_rejected:('b -> 'a) ->
  ('a, 'b) t ->
  ('a, 'b) t

val then_final :
  on_fulfilled:('a -> unit) ->
  on_rejected:('b -> unit) ->
  ('a, 'b) t ->
  unit

val to_lwt : ('a, exn) t -> 'a Lwt.t

val to_lwt_result : ('a, 'b) t -> ('a, 'b) result Lwt.t

val all : (('a, 'b) t) array -> ('a array, 'b) t

val race : (('a, 'b) t) array -> ('a, 'b) t

module Infix : sig
  val (>>=) : ('a, 'b) t -> ('a -> ('c ,'b) t) -> ('c, 'b) t
  val (>|=) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t

  val (>>~) : ('a, 'b) t -> ('b -> ('a, 'b) t) -> ('a, 'b) t
  val (>|~) : ('a, 'b) t -> ('b -> 'a) -> ('a, 'b) t

  val (>||) : ('a, 'b) t -> ('a -> unit) * ('b -> unit) -> unit
end
