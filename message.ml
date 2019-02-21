open Js_of_ocaml
open Utils

type js_string = Js.js_string Js.t

class type t =
  object
    method janus : js_string Js.readonly_prop
    method transaction : js_string Js.readonly_prop
    method session_id : Js.number Js.t Js.optdef_prop
    method token : js_string Js.optdef_prop
    method apisecret : js_string Js.optdef_prop
  end

let make ?(token : string option)
      ?(apisecret : string option)
      ?(session_id : int64 option)
      ~(janus : string)
      ~(transaction : string)
      () =
  let (t : t Js.t) =
    Js.Unsafe.coerce (
        object%js
          val janus = Js.string janus
          val transaction = Js.string transaction
        end) in
  Option.iter (fun x -> t##.token := Js.string x) token;
  Option.iter (fun x -> t##.apisecret := Js.string x) apisecret;
  Option.iter (fun x ->
      let v = Js.number_of_float @@ Int64.to_float x in
      t##.session_id := v) session_id;
  t
