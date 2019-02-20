open Js_of_ocaml

type t = Js.Unsafe.any

let _of (x : 'a) : t =
  Js.Unsafe.inject x

let of_jsobj (x : Jsobj.t) : t =
  _of @@ Js.Unsafe.obj x

let of_int (x : int) : t = _of x

let of_bool (x : bool) : t =
  _of @@ Js.bool x

let of_string (x : string) : t =
  _of @@ Js.string x

let of_option (f : 'a -> 'b) (x : 'a option) : t =
  _of @@ Js.Optdef.(map (option x) f)

let of_string_opt (x : string option) : t =
  of_option Js.string x

let of_int_opt (x : int option) : t =
  of_option (fun x -> x) x

let of_bool_opt (x : bool option) : t =
  of_option Js.bool x
