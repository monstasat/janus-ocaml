open Js_of_ocaml

type jsep = < > Js.t

type candidate = < > Js.t

type t =
  { token : string option
  ; apisecret : string option
  ; plugin : string option
  ; session_id : int64 option
  ; handle_id : int64 option
  ; opaque_id : string option
  ; jsep : jsep option
  ; candidate : candidate option
  ; janus : string
  ; transaction : string
  }

val to_yojson : t -> Yojson.Safe.json

val of_yojson : Yojson.Safe.json -> (t, string) result

val make :
  ?token:string ->
  ?apisecret:string ->
  ?plugin:string ->
  ?session_id:int64 ->
  ?handle_id:int64 ->
  ?opaque_id:string ->
  ?jsep:jsep ->
  ?candidate:candidate ->
  janus:string ->
  transaction:string ->
  unit ->
  t
