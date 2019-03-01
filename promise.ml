open Js_of_ocaml

type ('a, 'b) promise

type 'a resolve = 'a -> unit

type 'a reject = 'a -> unit

let promise_global = Js.Unsafe.global##._Promise

let is_supported () = Js.Optdef.test promise_global

let make f =
  Js.Unsafe.new_obj promise_global [|Js.Unsafe.inject f|]

let resolve value =
  Js.Unsafe.fun_call promise_global##.resolve [|Js.Unsafe.inject value|]

let reject value =
  Js.Unsafe.fun_call promise_global##.reject [|Js.Unsafe.inject value|]

let js_of_opt = function
  | Some value -> Js.Unsafe.inject value
  | None       -> Js.Unsafe.inject Js.undefined

let then_bind ~on_fulfilled ?on_rejected promise =
  Js.Unsafe.meth_call promise "then"
    [|Js.Unsafe.inject on_fulfilled; js_of_opt on_rejected|]

let then_map ~on_fulfilled ?on_rejected promise =
  Js.Unsafe.meth_call promise "then"
    [|Js.Unsafe.inject on_fulfilled; js_of_opt on_rejected|]

let catch_bind ~on_rejected promise =
  Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject on_rejected|]

let catch_map ~on_rejected promise =
  Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject on_rejected|]

let then_final ~on_fulfilled ~on_rejected promise =
  Js.Unsafe.meth_call promise "then"
    [|Js.Unsafe.inject on_fulfilled; Js.Unsafe.inject on_rejected|]

let all promises =
  let intermediate_promise =
    Js.Unsafe.fun_call promise_global##.all
      [|Js.Unsafe.inject (Js.array promises)|]
  in
  then_map
    ~on_fulfilled:(fun js_array -> Js.to_array js_array) intermediate_promise

let race promises =
  Js.Unsafe.fun_call promise_global##.race
    [|Js.Unsafe.inject (Js.array promises)|]

module Infix = struct
  let (>>=) promise on_fulfilled = then_bind ~on_fulfilled promise
  let (>|=) promise on_fulfilled = then_map  ~on_fulfilled promise

  let (>>~) promise on_rejected = catch_bind ~on_rejected promise
  let (>|~) promise on_rejected = catch_map  ~on_rejected promise

  let (>||) promise (on_fulfilled, on_rejected) =
    then_final ~on_fulfilled ~on_rejected promise
end
