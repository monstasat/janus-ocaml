open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Infix
open Utils
open Webrtc

type 'a frame = 'a Lwt_xmlHttpRequest.generic_http_frame

type error_ext =
  { response : string option
  ; error : string option
  ; timeout : int option
  }
and error = string * error_ext option

let make_error ?timeout ?error ?response ?message ()
    : ('a, error) result =
  let message = match message with
    | None | Some "" -> "Internal error"
    | Some s -> s in
  let ext = match timeout, error, response with
    | None, None, None -> None
    | _ -> Some { response; error; timeout } in
  Error (message, ext)

let error_to_string ((text, ext) : error) : string =
  let option_to_string k f v = match v with
    | None -> ""
    | Some r ->
       match f r with
       | "" -> ""
       | s -> Printf.sprintf "%s = %s" k s in
  match ext with
  | None -> text
  | Some { response; error; timeout } ->
     String.concat ", "
     @@ List.filter (function "" -> false | _ -> true)
          [ option_to_string "response" (fun x -> x) response
          ; option_to_string "error" (fun x -> x) error
          ; option_to_string "timeout" (Printf.sprintf "%d") timeout ]
     |> function "" -> text | s -> Printf.sprintf "%s: %s" text s

module Msg = struct

  (* Misc *)

  class type err =
    object
      (** A numeric error code *)
      method code : int Js.prop
      (** A verbose string describing the cause of the failure *)
      method reason : Js.js_string Js.t Js.prop
    end

  class type plugindata =
    object
      (** The plugin's unique package name *)
      method plugin : Js.js_string Js.t Js.prop
      (** An opaque JSON object that is plugin specific *)
      method data : 'a Js.t Js.prop
    end

  class type candidate =
    object
      method completed : bool Js.t Js.optdef_prop
      method candidate : Js.js_string Js.t Js.optdef_prop
      method sdpMid : Js.js_string Js.t Js.optdef_prop
      method sdpMLineIndex : int Js.opt Js.optdef_prop
    end

  class type data =
    object
      method id : int Js.prop
    end

  (* Messages *)

  class type msg =
    object
      (* Required *)
      method janus : Js.js_string Js.t Js.prop
      (* Optional *)
      method error : err Js.t Js.optdef_prop
      method transaction : Js.js_string Js.t Js.optdef_prop
      method token : Js.js_string Js.t Js.optdef_prop
      method apisecret : Js.js_string Js.t Js.optdef_prop
    end

  class type req =
    object
      inherit msg
      method plugin : Js.js_string Js.t Js.optdef_prop
      method opaque_id : Js.js_string Js.t Js.optdef_prop
      method session_id : int Js.optdef_prop
      method handle_id : int Js.optdef_prop
      method jsep : _RTCSessionDescription Js.t Js.optdef_prop
      method body : 'a Js.t Js.optdef_prop
      method candidate : candidate Js.t Js.optdef_prop
      method candidates : candidate Js.t Js.js_array Js.t Js.optdef_prop
    end

  class type rsp_id =
    object
      inherit msg
      method data : data Js.t Js.prop
    end

  class type handle_event =
    object
      inherit msg
      (** Unique numeric plugin handle identifier *)
      method sender : int Js.optdef Js.readonly_prop
      (** A JSON object containing the info coming from the plugin *)
      method plugindata : plugindata Js.t Js.optdef Js.readonly_prop
      (** An optional JSON object containing the JSEP SDP (offer or answer)
          the plugin may send to negotiate a WebRTC PeerConnection with the
          client *)
      method jsep : _RTCSessionDescription Js.t Js.optdef_prop
    end

  class type webrtc_event_base =
    object
      inherit msg
      (** The session identifier *)
      method session_id : int Js.optdef_prop
      (** Unique numeric plugin handle identifier *)
      method sender : int Js.optdef Js.readonly_prop
    end

  class type webrtc_event_media =
    object
      inherit webrtc_event_base
      (** Type of media: audio/video *)
      method _type : Js.js_string Js.t Js.readonly_prop
      (** Wheter Janus is receiving media of type on this PeerConnection *)
      method receiving : bool Js.t Js.readonly_prop
    end

  class type webrtc_event_slowlink =
    object
      inherit webrtc_event_base
      (** Uplink from Janus' perspective
          true - problem sending video
          false - problem receiving video *)
      method uplink : bool Js.t Js.readonly_prop
      (** Number of NACKs in the last second *)
      method nacks : int Js.readonly_prop
    end

  class type webrtc_event_hangup =
    object
      inherit webrtc_event_base
      method reason : Js.js_string Js.t Js.readonly_prop
    end

  let make_req ?(token : string option)
        ?(apisecret : string option)
        ?(plugin : string option)
        ?(session_id : int option)
        ?(handle_id : int option)
        ?(opaque_id : string option)
        ?(jsep : _RTCSessionDescription Js.t option)
        ?(candidate : candidate Js.t option)
        ?(body : 'a Js.t option)
        ~(janus : string)
        ~(transaction : string)
        () : req Js.t =
    let (o : req Js.t) = Js.Unsafe.obj [||] in
    o##.janus := Js.string janus;
    o##.transaction := Js.string transaction;
    Option.(
      iter (fun x -> o##.token := Js.string x) token;
      iter (fun x -> o##.apisecret := Js.string x) apisecret;
      iter (fun x -> o##.plugin := Js.string x) plugin;
      iter (fun x -> o##.session_id := x) session_id;
      iter (fun x -> o##.handle_id := x) handle_id;
      iter (fun x -> o##.opaque_id := Js.string x) opaque_id;
      iter (fun x -> o##.jsep := x) jsep;
      iter (fun x -> o##.candidate := x) candidate;
      iter (fun x -> o##.body := x) body);
    o

  let to_string (t : #msg Js.t) : string =
    Js.to_string @@ Json.output t

  let check_err_map ~(ok : (string * (msg Js.t -> 'a)) list)
        (msg : #msg Js.t) : ('a, string) result =
    Printf.(
      try
        match Js.to_string msg##.janus with
        | "error" ->
           (match Js.Optdef.to_option msg##.error with
            | None -> Error "Oops: internal error"
            | Some (err : err Js.t) ->
               let reason = Js.to_string err##.reason in
               Error (sprintf "Oops: %d %s" err##.code reason))
        | janus ->
           (match List.find_opt (fun (k, _) -> String.equal k janus) ok with
            | Some (_, f) -> Ok (f (msg :> msg Js.t))
            | None -> Error (sprintf "Unexpected response: %s" janus))
      with e -> Error (sprintf "Probably a parser error: %s"
                       @@ Printexc.to_string e))
    |> function Error e -> Log.ign_error e; Error e | Ok x -> Ok x

  let check_err ?(ok = ["success"])
        (msg : #msg Js.t) : (msg Js.t, string) result =
    let ok = List.map (fun k -> k, (fun x -> x)) ok in
    check_err_map ~ok msg

  let of_frame (frame : 'a Js.opt frame) : ('a Js.t, error) result =
    match frame.code, Js.Opt.to_option frame.content with
    | 0, _ ->
       let message = "Probably a network error, is the server down?" in
       Log.ign_error message;
       make_error ~message ()
    | 200, None ->
       let message = "Got empty JSON" in
       Log.ign_error message;
       make_error ~message ()
    | 200, Some x -> Ok (Obj.magic x)
    | code, response ->
       let response = match response with
         | None -> None
         | Some json -> Some (Js.to_string @@ Json.output json) in
       let message = Printf.sprintf "API call failed (%d)" code in
       Log.ign_error message;
       make_error ?response ~message ()

end

let http_call ?async ?timeout ?with_credentials
      ?(body : #Msg.msg Js.t option)
      ~meth
      (uri : string)
    : ('a Js.t, error) Lwt_result.t =
  ignore async;
  let content_type = match meth with
    | `POST -> Some ("Content-Type", "application/json")
    | _ -> None in
  let (headers : (string * string) list) =
    ["Accept", "application/json, text/plain, */*"]
    |> List.cons_maybe content_type in
  let contents = match body with
    | None -> None
    | Some x -> Some (`String (Msg.to_string x)) in
  let (frame : 'a Js.opt frame Lwt.t) =
    Lwt_xmlHttpRequest.perform_raw
      ?with_credentials
      ?contents
      ~headers
      ~override_method:meth
      ~response_type:XmlHttpRequest.JSON
      uri in
  Lwt.catch (fun () ->
      let (t : ('a Js.t, error) Lwt_result.t) = frame >|= Msg.of_frame in
      match timeout with
      | None -> t
      | Some (x : int) ->
         let sleep =
           Lwt_js.sleep @@ (float_of_int x /. 1000.)
           >|= (make_error ~timeout:x ~message:"Request timed out") in
         Lwt.choose [t; sleep])
    (fun exn -> Lwt.return @@ make_error ~message:(Printexc.to_string exn) ())
