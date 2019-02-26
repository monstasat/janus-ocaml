open Js_of_ocaml
open Media_stream
open Utils

class type candidate =
  object
    method completed : bool Js.t Js.optdef_prop
    method candidate : _RTCIceCandidate Js.t Js.optdef_prop
    method sdpMid : Js.js_string Js.t Js.optdef_prop
    method sdpMLineIndex : int Js.optdef_prop
  end

class type t =
  object
    method token : Js.js_string Js.t Js.optdef_prop
    method apisecret : Js.js_string Js.t Js.optdef_prop
    method plugin : Js.js_string Js.t Js.optdef_prop
    method session_id : int Js.optdef_prop
    method handle_id : int Js.optdef_prop
    method opaque_id : Js.js_string Js.t Js.optdef_prop
    method jsep : 'a Js.t Js.optdef_prop
    method candidate : candidate Js.t Js.optdef_prop
    method janus : Js.js_string Js.t Js.optdef_prop
    method transaction : Js.js_string Js.t Js.optdef_prop
  end

let to_json (t : t Js.t) : string =
  Js.to_string @@ Json.output t

let make ?(token : string option)
      ?(apisecret : string option)
      ?(plugin : string option)
      ?(session_id : int64 option)
      ?(handle_id : int64 option)
      ?(opaque_id : string option)
      ?(jsep : _RTCSessionDescription Js.t option)
      ?(candidate : candidate Js.t option)
      ~(janus : string)
      ~(transaction : string)
      () : t Js.t =
  let (o : t Js.t) = Js.Unsafe.obj [||] in
  o##.janus := Js.string janus;
  o##.transaction := Js.string transaction;
  Option.(
    iter (fun x -> o##.token := Js.string x) token;
    iter (fun x -> o##.apisecret := Js.string x) apisecret;
    iter (fun x -> o##.plugin := Js.string x) plugin;
    iter (fun x -> o##.session_id := Int64.to_int x) session_id;
    iter (fun x -> o##.handle_id := Int64.to_int x) handle_id;
    iter (fun x -> o##.opaque_id := Js.string x) opaque_id;
    iter (fun x -> o##.jsep := x) jsep;
    iter (fun x -> o##.candidate := x) candidate);
  o

type response =
  { janus : string
  ; transaction : string
  ; session_id : int64 option [@default None]
  ; data : data option [@default None]
  ; error : error option [@default None]
  }
and error =
  { reason : string
  ; code : int
  }
and data =
  { id : int64
  } [@@deriving yojson { strict = false }]

let parse_response ?(janus_ok = "success") = function
  | Ok None ->
     let s = "The response is empty" in
     Logs.ign_error s;
     Error s
  | Error e ->
     let s = Api.error_to_string e in
     Logs.ign_error s;
     Error s
  | Ok Some json ->
     Logs.ign_debug ~inspect:json "got response:";
     match response_of_yojson json with
     | Error e ->
        Logs.ign_error ~inspect:e "Bad json format:";
        Error e
     | Ok ({ janus; _ } as rsp) when String.equal janus_ok janus -> Ok rsp
     | Ok { error = None; _ } ->
        let reason = "Unknown error" in
        Logs.ign_error reason;
        Error reason
     | Ok { error = Some e; _ } ->
        Logs.ign_error_f "Oops: %d %s" e.code e.reason;
        Error e.reason
