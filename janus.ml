open Js_of_ocaml
open Utils

exception Not_created of string

type media_stream

let ( >>= ) = Lwt.( >>= )

let is_some = function None -> false | _ -> true

let int_of_number x = int_of_float @@ Js.float_of_number x

let wakeup_exn (w : 'a Lwt.u) (s : string) : unit =
  Lwt.wakeup_exn w (Not_created s)

let call_js_method f (params : Jsobj.t) =
  let open Js.Unsafe in
  let t, w = Lwt.wait () in
  let success =
    ("success",
     inject @@ Js.wrap_callback (fun x -> Lwt.wakeup w x)) in
  let error =
    ("error",
     inject @@ Js.wrap_callback (fun err ->
                   Lwt.wakeup_exn w (Failure (Js.to_string (Json.output err))))) in
  f Jsobj.(to_js @@ make [success; error] @ params);
  Lwt.catch
    (fun () -> t >>= (fun x -> Lwt.return_ok x))
    (function
     | Failure e -> Lwt.return_error e
     | e -> Lwt.return_error (Printexc.to_string e))

(** Janus plugin handler **)
module Plugin = struct

  (* Types *)

  (** Plugin handle object API **)
  class type plugin =
    object
      method getId : unit -> Js.number Js.t Js.meth
      method getPlugin : unit -> Js.js_string Js.t Js.meth
      method send : 'a Js.t -> unit Js.meth
      method createAnswer : 'a Js.t -> unit Js.meth
      method createOffer : 'a Js.t -> unit Js.meth
      method handleRemoteJsep : 'a Js.t -> unit Js.meth
      method dtmf : 'a Js.t -> unit Js.meth
      method data : 'a Js.t -> unit Js.meth
      method getBitrate : unit -> Js.js_string Js.t Js.meth
      method hangup : bool Js.t -> unit Js.meth
      method detach : 'a Js.t -> unit Js.meth
    end

  type e = string

  type typ =
    | Audiobridge
    | Echotest
    | Recordplay
    | Sip
    | Streaming
    | Textroom
    | Videocall
    | Videoroom
    | Voicemail

  type media_video =
    | Bool of bool
    | Resolution of ([`Lowres | `Stdres | `Hires] * [`Wide | `Square ])
    | Screen
    | Device of (int * int * int)

  type media_audio =
    | Bool of bool
    | Device of int

  type media_props =
    { audio_send : bool option
    ; audio_recv : bool option
    ; audio : media_audio option
    ; video_send : bool option
    ; video_recv : bool option
    ; video : media_video option
    ; data : bool option
    ; fail_if_no_video : bool option
    ; fail_if_no_audio : bool option
    ; screen_rate : int option
    }

  type 'a send_response =
    | Error of int option * string
    | Empty
    | Data of 'a

  (* Helper functions *)

  let typ_to_string type_ =
    let name = match type_ with
      | Audiobridge -> "audiobridge"
      | Echotest -> "echotest"
      | Recordplay -> "recordplay"
      | Sip -> "sip"
      | Streaming -> "streaming"
      | Textroom -> "textroom"
      | Videocall -> "videocall"
      | Videoroom -> "videoroom"
      | Voicemail -> "voicemail" in
    "janus.plugin." ^ name

  let get_resolution_string quality aspect =
    let q = match quality with
      | `Lowres -> "lowres"
      | `Stdres -> "stdres"
      | `Hires -> "hires" in
    let a = match aspect with
      | `Wide -> "-16:9"
      | `Square -> "" in
    q ^ a

  let parse_sync_response name f = function
    | Empty -> Result.Error "Empty message"
    | Data d ->
       let prop = Js.Unsafe.get d name in
       Js.Optdef.bind prop (fun x -> Js.Unsafe.get d x)
       |> Js.Optdef.to_option
       |> (function
           | Some x -> f x
           | None -> Result.Error "Bad response")
    | Error (_, msg) -> Result.Error msg

  let parse_async_response = function
    | Empty -> Ok ()
    | Error (_, msg) -> Result.Error msg (* FIXME add error code to error string? *)
    | Data _ -> Result.Error "Data returned in async request"

  let data_or_error response =
    let response = Js.Optdef.to_option response in
    match response with
    | Some response ->
       if (Js.Optdef.test ((Js.Unsafe.coerce response)##.error_code)) ||
            (Js.Optdef.test ((Js.Unsafe.coerce response)##.error))
       then
         let error = match Js.Optdef.to_option (Js.Unsafe.coerce response)##.error with
           | Some x -> Js.to_string x
           | None -> "" in
         let code  = match Js.Optdef.to_option (Js.Unsafe.coerce response)##.error_code with
           | Some x -> Some (int_of_number x)
           | None -> None in
         Error (code, error)
       else Data response
    | None -> Empty

  let prepare_offer_or_answer ?jsep media trickle =
    let open Js.Unsafe in
    let media' =
      ("media",
       [ ("audioSend", Any.of_bool_opt media.audio_send);
         ("audioRecv", Any.of_bool_opt media.audio_recv);
         ("audio",
          match media.audio with
          | Some x ->
             begin match x with
             | Bool x -> Js.bool x |> inject
             | Device id -> obj [| ("deviceId", Js.Unsafe.inject id) |] |> inject
             end
          | None -> inject Js.Optdef.empty);
         ("videoSend", Any.of_bool_opt media.video_send);
         ("videoRecv", Any.of_bool_opt media.video_recv);
         ("video",
          match media.video with
          | Some x ->
             begin match x with
             | Bool x -> Js.bool x |> inject
             | Resolution (quality, aspect) ->
                get_resolution_string quality aspect |> Js.string |> inject
             | Screen -> Js.string "screen" |> Js.Unsafe.inject
             | Device (id, w, h) ->
                obj [| ("deviceId", inject id);
                       ("width", inject w);
                       ("height", inject h) |] |> inject
             end
          | None -> Js.Unsafe.inject Js.Optdef.empty);
         ("data", Any.of_bool_opt media.data);
         ("failIfNoVideo", Any.of_bool_opt media.fail_if_no_video);
         ("failIfNoAudio", Any.of_bool_opt media.fail_if_no_audio);
         ("screenShareFrameRate", Any.of_int_opt media.screen_rate) ]
       |> Array.of_list
       |> Any.of_jsobj) in
    [| media'
     ; ("trickle", Any.of_bool_opt trickle)
    |]
    |> (fun a ->
      if is_some jsep
      then Jsobj.(append a (singleton "jsep" (Any._of jsep)))
      else a)

  class t ~(server : string) ~(session_id : int64) (p : plugin Js.t) =
  object(self)

    method id : int64 =
      Int64.of_float @@ Js.float_of_number @@ p##getId ()

    method name : string =
      Js.to_string @@ p##getPlugin ()

    method send : 'a 'b 'c 'd.
                  ?jsep:'a ->
                  'b ->
                  ('b -> Jsobj.t) ->
                  ('c -> 'b -> ('d, string) result) ->
                  ('d, string) Lwt_result.t =
      fun ?jsep request request_to_obj parse_response ->
      let transaction = String.random 12 in
      let jsep = match jsep with
        | None -> None
        | Some j -> Some ("jsep", Any._of j) in
      let body =
        [ ("janus", Any.of_string "message")
        ; ("body", Any.of_jsobj @@ request_to_obj request)
        ; ("transaction", Any.of_string transaction)
        ]
        |> List.cons_maybe jsep
        |> Jsobj.make
        |> Js.Unsafe.obj in
      let url = Printf.sprintf "%s/%Ld/%Ld" server session_id self#id in
      Lwt.(
        Api.http_api_call ~meth:`POST ~body url
        >|= function
        | Ok x -> parse_response (Obj.magic x) request
        | Error _ -> Error "")

    method create_answer ?(trickle : bool option)
             (media : media_props) (jsep : Js.json Js.t)
           : (Js.json Js.t, string) Lwt_result.t =
      prepare_offer_or_answer ~jsep:jsep media trickle
      |> call_js_method (fun x -> p##createAnswer x)

    method create_offer ?(trickle : bool option) (media : media_props)
           : (unit, string) Lwt_result.t =
      prepare_offer_or_answer media trickle
      |> call_js_method (fun x -> p##createOffer x)

    method handle_remote_jsep (jsep : Js.json Js.t)
           : (unit, string) Lwt_result.t =
      call_js_method (fun x -> p##handleRemoteJsep x)
      @@ Jsobj.singleton "jsep" (Any._of jsep)

    method dtmf ?(duration : int option) ?(gap : int option) (tones : string)
           : (unit, string) Lwt_result.t =
      call_js_method (fun x -> p##dtmf x)
      @@ Jsobj.make [ ("tones", Any.of_string tones)
                    ; ("duration", Any.of_int_opt duration)
                    ; ("gap", Any.of_int_opt gap) ]

    method data (text : string) : (unit, string) Lwt_result.t =
      call_js_method (fun x -> p##data x)
      @@ Jsobj.singleton "text" (Any.of_string text)

    method get_bitrate () : string =
      Js.to_string @@ p##getBitrate ()

    method hangup ?(send_request = false) () : unit =
      p##hangup (Js.bool send_request)

    method detach () : (unit, string) Lwt_result.t =
      call_js_method (fun x -> p##detach x) Jsobj.empty

  end

end

module Session = struct

  class type session =
    object
      method getServer : unit -> Js.js_string Js.t Js.meth
      method isConnected : unit -> bool Js.t Js.meth
      method getSessionId : unit -> Js.number Js.t Js.meth
      method attach : 'a Js.t -> unit Js.meth
      method destroy : 'a Js.t -> unit Js.meth
    end

  type e =
    | Err of string
    | Destroyed

  type jsep =
    | Offer of Js.json Js.t
    | Answer of Js.json Js.t
    | Unknown of Js.json Js.t

  let handle_message (plugin : Plugin.t Lwt.t) on_msg on_jsep msg jsep =
    match Lwt.poll plugin with
    | None ->
       (* Unreachable case since promise should be fullfilled by the time
          plugin starts sending messages *)
       assert false
    | Some (plugin : Plugin.t) ->
       let jsep' =
         Js.Optdef.to_option jsep
         |> function None -> None
                   | Some x -> Js.Opt.to_option x in
       (* Handle message *)
       (match on_msg with
        | None -> ()
        | Some f -> f plugin msg);
       (* Handle jsep if some *)
       (match jsep', on_jsep with
        | Some jsep, Some f ->
           let jsep = match Js.to_string (Js.Unsafe.coerce jsep)##.type_ with
             | "offer" -> Offer jsep
             | "answer" -> Answer jsep
             | _ -> Unknown jsep in
           f plugin jsep
        | _ -> ())

  let wrap_cb = fun ?f name f_push ->
    name,
    match f with
    | None -> Any.of_option Js.wrap_callback f_push
    | Some f ->
       Any.of_option (fun push ->
           Js.wrap_callback (fun data -> push @@ f data)) f_push

  type stream_callback = media_stream Js.t -> unit

  class t (s : session Js.t) =
  object(self)

    method id : int64 =
      Int64.of_float @@ Js.float_of_number @@ s##getSessionId ()

    method server : string =
      Js.to_string @@ s##getServer ()

    method connected : bool =
      Js.to_bool @@ s##isConnected ()

    method attach ~typ ?opaque_id
             ?(on_local_stream : stream_callback option)
             ?(on_remote_stream : stream_callback option)
             ?(on_message : (Plugin.t -> Js.json Js.t -> unit) option)
             ?(on_jsep : (Plugin.t -> jsep -> unit) option)
             ?(consent_dialog : (bool -> unit) option)
             ?(webrtc_state : (bool -> unit) option)
             ?(ice_state : (string -> unit) option)
             ?(media_state : (string * bool -> unit) option)
             ?(slow_link : (bool -> unit) option)
             ?(on_cleanup : (unit -> unit) option)
             ?(detached : (unit -> unit) option)
             ()
           : (Plugin.t * Plugin.e React.event) Lwt.t =
      let inject = Js.Unsafe.inject in
      let t, w = Lwt.wait () in
      let e, set_e = React.E.create () in
      let t = Lwt.(
          t >|= (fun x ->
            new Plugin.t ~session_id:self#id ~server:self#server x)) in
      let on_error = fun e ->
        let s = Js.to_string e in
        if Lwt.is_sleeping t
        then wakeup_exn w s
        else set_e s in
      let on_message' = handle_message t on_message on_jsep in
      let to_media_state = fun (s, b) -> Js.(to_string s, to_bool b) in
      [| ("plugin", Any.of_string @@ Plugin.typ_to_string typ)
       ; ("opaqueId", Any.of_string_opt opaque_id)
       ; ("onmessage", inject @@ Js.wrap_callback on_message')
       ; wrap_cb "consentDialog" consent_dialog ~f:Js.to_bool
       ; wrap_cb "webrtcState" webrtc_state ~f:Js.to_bool
       ; wrap_cb "iceState" ice_state ~f:Js.to_string
       ; wrap_cb "mediaState" media_state ~f:to_media_state
       ; wrap_cb "slowLink" slow_link ~f:Js.to_bool
       ; wrap_cb "oncleanup" on_cleanup
       ; wrap_cb "detached" detached
       ; wrap_cb "onlocalstream" on_local_stream
       ; wrap_cb "onremotestream" on_remote_stream
       ; ("success", inject @@ Js.wrap_callback (Lwt.wakeup w))
       ; ("error", inject @@ Js.wrap_callback on_error)
      |]
      |> Js.Unsafe.obj
      |> (fun obj -> s##attach obj);
      Lwt.(t >|= (fun t -> t, e))

    method destroy () : unit =
      call_js_method (fun x -> s##destroy x) [||]
      |> Lwt.ignore_result

  end

  let create ~server ?ice_servers ?ipv6 ?with_credentials
        ?max_poll_events ?destroy_on_unload ?token ?apisecret ()
      : (t * e React.event) Lwt.t =
    let inject = Js.Unsafe.inject in
    let t, w = Lwt.wait () in
    let (e : e React.event), set_e = React.E.create () in
    let server = match server with
      | `One x -> Any.of_string x
      | `Many x -> inject @@ to_js_string_array x in
    let on_error = fun e ->
      let s = Js.to_string e in
      print_endline s;
      if Lwt.is_sleeping t
      then wakeup_exn w s
      else set_e (Err s) in
    let j =
      [| ("server", server)
       ; ("iceServers", Any.of_option to_js_string_array ice_servers)
       ; ("ipv6", Any.of_bool_opt ipv6)
       ; ("withCredentials", Any.of_bool_opt with_credentials)
       ; ("max_poll_events", Any.of_int_opt max_poll_events)
       ; ("destroyOnUnload", Any.of_bool_opt destroy_on_unload)
       ; ("token", Any.of_string_opt token)
       ; ("apisecret", Any.of_string_opt apisecret)
       ; ("success", inject @@ Js.wrap_callback (Lwt.wakeup w))
       ; ("error", inject @@ Js.wrap_callback on_error)
       ; ("destroy", inject @@ Js.wrap_callback (fun () -> set_e Destroyed))
      |]
      |> (fun x -> Js.Unsafe.(new_obj global##._Janus [|obj x|])) in
    t >>= (fun () -> Lwt.return (new t j, e))

end

let is_webrtc_supported () : bool =
  let test_def = Js.Optdef.test in
  let test_opt = Js.Opt.test in
  let coerce = Js.Unsafe.coerce in
  let wnd = Dom_html.window in
  let nav = wnd##.navigator in
  test_def (coerce wnd)##.RTCPeerConnection
  && test_opt (coerce wnd)##.RTCPeerConnection
  && test_def (coerce nav)##.getUserMedia
  && test_opt (coerce nav)##.getUserMedia

let attach_media_stream (elt : #Dom_html.mediaElement Js.t)
      (stream : media_stream Js.t) : unit =
  let elt = (elt :> Dom_html.mediaElement Js.t) in
  let details = Adapter.adapter.browser_details in
  match details.browser with
  | "chrome" ->
     let src_type = Js.to_string @@ Js.typeof elt##.src in
     if details.version >= 52
     then (Js.Unsafe.coerce elt)##.srcObject := stream
     else if (not @@ String.equal "undefined" src_type)
     then (
       let url = Dom_html.window##._URL in
       elt##.src := url##createObjectURL (Js.Unsafe.coerce stream))
     else Logs.err (fun m -> m "Rrror attaching stream to element")
  | _ -> (Js.Unsafe.coerce elt)##.srcObject := stream

let reattach_media_stream ~(from : #Dom_html.mediaElement Js.t)
      (_to : #Dom_html.mediaElement Js.t) : unit =
  let from = (from :> Dom_html.mediaElement Js.t) in
  let _to = (_to :> Dom_html.mediaElement Js.t) in
  let details = Adapter.adapter.browser_details in
  let swap_obj ~from _to =
    let from_obj = (Js.Unsafe.coerce from)##.scrObject in
    (Js.Unsafe.coerce _to)##.scrObject := from_obj in
  match details.browser with
  | "chrome" ->
     let src_type = Js.to_string @@ Js.typeof _to##.src in
     if details.version >= 52
     then swap_obj ~from _to
     else if (not @@ String.equal "undefined" src_type)
     then _to##.src := from##.src
     else Logs.err (fun m -> m "Error reattaching stream to element")
  | _ -> swap_obj ~from _to

include Instance
