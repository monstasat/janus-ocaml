type ('a, 'b) promise

type 'a resolve = 'a -> unit

type 'a reject = 'a -> unit

val is_supported : unit -> bool

val make : ('a resolve -> 'b reject -> unit) -> ('a, 'b) promise

val resolve : 'a -> ('a, 'b) promise

val reject : 'b -> ('a, 'b) promise

val then_bind :
  on_fulfilled:('a -> ('c ,'b) promise) ->
  ?on_rejected:('b -> ('c, 'b) promise) ->
  ('a, 'b) promise ->
  ('c, 'b) promise

val then_map :
  on_fulfilled:('a -> 'c) ->
  ?on_rejected:('b -> 'c) ->
  ('a, 'b) promise ->
  ('c, 'b) promise

val catch_bind :
  on_rejected:('b -> ('a, 'b) promise) ->
  ('a, 'b) promise ->
  ('a, 'b) promise

val catch_map :
  on_rejected:('b -> 'a) ->
  ('a, 'b) promise ->
  ('a, 'b) promise

val then_final :
  on_fulfilled:('a -> unit) ->
  on_rejected:('b -> unit) ->
  ('a, 'b) promise ->
  unit

val all : (('a, 'b) promise) array -> ('a array, 'b) promise

val race : (('a, 'b) promise) array -> ('a, 'b) promise

module Infix : sig
  val (>>=) : ('a, 'b) promise -> ('a -> ('c ,'b) promise) -> ('c, 'b) promise
  val (>|=) : ('a, 'b) promise -> ('a -> 'c) -> ('c, 'b) promise

  val (>>~) : ('a, 'b) promise -> ('b -> ('a, 'b) promise) -> ('a, 'b) promise
  val (>|~) : ('a, 'b) promise -> ('b -> 'a) -> ('a, 'b) promise

  val (>||) : ('a, 'b) promise -> ('a -> unit) * ('b -> unit) -> unit
end
