open Js_of_ocaml

type jsep = < > Js.t

type candidate = < > Js.t

let jsep_to_yojson (jsep : jsep) : Yojson.Safe.json =
  `String (Js.to_string @@ Json.output jsep)

let jsep_of_yojson (json : Yojson.Safe.json) : (jsep, string) result =
  match json with
  | `String s ->
     (try Ok (Json.unsafe_input (Js.string s))
      with e -> Error (Printexc.to_string e))
  | _ -> Error "jsep_of_yojson: bad json"

let candidate_to_yojson (jsep : candidate) : Yojson.Safe.json =
  `String (Js.to_string @@ Json.output jsep)

let candidate_of_yojson (json : Yojson.Safe.json) : (candidate, string) result =
  match json with
  | `String s ->
     (try Ok (Json.unsafe_input (Js.string s))
      with e -> Error (Printexc.to_string e))
  | _ -> Error "candidate_of_yojson: bad json"

type t =
  { token : string option [@default None]
  ; apisecret : string option [@default None]
  ; plugin : string option [@default None]
  ; session_id : int64 option [@default None]
  ; handle_id : int64 option [@default None]
  ; opaque_id : string option [@default None]
  ; jsep : jsep option [@default None]
  ; candidate : candidate option [@default None]
  ; janus : string
  ; transaction : string
  } [@@deriving yojson { strict = false }]

let make ?(token : string option)
      ?(apisecret : string option)
      ?(plugin : string option)
      ?(session_id : int64 option)
      ?(handle_id : int64 option)
      ?(opaque_id : string option)
      ?(jsep : jsep option)
      ?(candidate : candidate option)
      ~(janus : string)
      ~(transaction : string)
      () : t =
  { token
  ; apisecret
  ; plugin
  ; session_id
  ; handle_id
  ; opaque_id
  ; jsep
  ; candidate
  ; janus
  ; transaction
  }
