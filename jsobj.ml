open Js_of_ocaml

type k = string
type v = Js.Unsafe.any

type t = (k * v) array

type t_js

let empty : t = [||]

let singleton (k : k) (v : v) : t =
  [|(k, v)|]

let make (l : (k * v) list) : t =
  Array.of_list @@ List.filter (fun (_, v) -> Js.Optdef.test (Obj.magic v)) l

let concat (l : t list) : t =
  Array.concat l

let append (a : t) (b : t) : t =
  Array.append a b

let to_js (t : t) : t_js =
  Js.Unsafe.obj t

let ( @ ) = append
