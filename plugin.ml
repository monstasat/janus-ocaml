open Js_of_ocaml
open Utils
open Types
open Media_stream
open Adapter
open Lwt.Infix

type ice_connection_state =
  | New
  | Checking
  | Connected
  | Completed
  | Failed
  | Disconnected
  | Closed
  | Unknown

let ice_connection_state_of_string : string -> ice_connection_state = function
  | "new" -> New
  | "checking" -> Checking
  | "connected" -> Connected
  | "completed" -> Completed
  | "failed" -> Failed
  | "disconnected" -> Disconnected
  | "closed" -> Closed
  | _ -> Unknown

let ice_connection_state_to_string : ice_connection_state -> string = function
  | New -> "new"
  | Checking -> "checking"
  | Connected -> "connected"
  | Completed -> "completed"
  | Failed -> "failed"
  | Disconnected -> "disconnected"
  | Closed -> "closed"
  | Unknown -> "unknown"

type callbacks =
  { on_local_stream : (mediaStream Js.t -> unit) option
  ; on_remote_stream : (mediaStream Js.t -> unit) option
  ; on_message : unit -> unit
  ; on_jsep : unit -> unit
  ; consent_dialog : bool -> unit
  ; ice_state : (ice_connection_state -> unit) option
  ; webrtc_state : bool -> unit
  ; media_state : (string * bool) -> unit
  ; slow_link : bool -> unit
  ; on_data : (string -> unit) option (* FIXME check type *)
  ; on_data_open : (unit -> unit) option
  ; on_data_error : 'a. ('a Js.t -> unit) option (* FIXME add type *)
  ; on_cleanup : unit -> unit
  ; detached : unit -> unit
  }

type properties =
  { opaque_id : string
  ; token : string option
  }

module Webrtc_stuff = struct

  type volume =
    { value : unit option
    ; timer : unit option
    }

  type bitrate =
    { value : unit option
    ; bsnow : unit option
    ; bsbefore : unit option
    ; tsnow : unit option
    ; tsbefore : unit option
    ; timer : unit option
    }

  type t =
    { started : bool
    ; local_stream : mediaStream Js.t option
    ; stream_external : bool
    ; remote_stream : mediaStream Js.t option
    ; local_sdp : _RTCSessionDescription Js.t option
    ; media_constraints : unit option
    ; pc : _RTCPeerConnection Js.t option
    ; data_channel : 'a. 'a Js.t option
    ; dtmf_sender : unit option
    ; trickle : bool
    ; ice_done : bool
    ; sdp_sent : bool
    ; volume : volume
    ; bitrate : bitrate
    }

  let make_empty () : t =
    { started = false
    ; local_stream = None
    ; stream_external = false
    ; remote_stream = None
    ; local_sdp = None
    ; media_constraints = None
    ; pc = None
    ; data_channel = None
    ; dtmf_sender = None
    ; trickle = true
    ; ice_done = false
    ; sdp_sent = false
    ; volume =
        { value = None
        ; timer = None
        }
    ; bitrate =
        { value = None
        ; bsnow = None
        ; bsbefore = None
        ; tsnow = None
        ; tsbefore = None
        ; timer = None
        }
    }

end

module Media = struct

  type video =
    | Bool of bool
    | Resolution of ([`Lowres | `Stdres | `Hires] * [`Wide | `Square ])
    | Screen
    | Device of video_device
  and video_device =
    { device_id : int
    ; width : int
    ; height : int
    }

  type audio =
    | Bool of bool
    | Device of audio_device
  and audio_device =
    { device_id : int
    }

  type t =
    { audio_send : bool option
    ; audio_recv : bool option
    ; audio : audio option
    ; video_send : bool option
    ; video_recv : bool option
    ; video : video option
    ; data : bool option
    ; fail_if_no_video : bool option
    ; fail_if_no_audio : bool option
    ; screen_share_frame_rate : int option

    ; update : bool
    ; add_video : bool
    ; keep_video : bool
    ; remove_video : bool
    ; replace_video : bool
    ; add_audio : bool
    ; keep_audio : bool
    ; remove_audio : bool
    ; replace_audio : bool
    ; add_data : bool
    }

end

type t =
  { id : int64
  ; server : Uri.t
  ; session_id : int64
  ; plugin : string
  ; token : string option
  ; apisecret : string option
  ; with_credentials : bool
  ; ice_servers : _RTCIceServer Js.t list
  ; ice_transport_policy : ice_transport_policy option
  ; bundle_policy : bundle_policy option
  ; callbacks : callbacks
  ; mutable webrtc_stuff : Webrtc_stuff.t
  ; mutable detached : bool
  }

let is_data_enabled ?(media : Media.t option) () : bool =
  match get_browser () with
  | "edge" ->
     Logs.ign_warning "Edge doen't support data channels yet";
     false
  | _ ->
     match media with
     | None | Some { data = None; _ }-> false (* Default *)
     | Some { data = Some b; _ } -> b

let is_audio_send_enabled ?(media : Media.t option) () : bool =
  match media with
  | None -> true (* Default *)
  | Some (media : Media.t) ->
     match media.audio, media.audio_send with
     | Some Bool false, _ -> false (* Generic audio has precedence *)
     | _, None -> true (* Default *)
     | _, Some x -> x

let is_video_send_enabled ?(media : Media.t option) () : bool =
  match media with
  | None -> true
  | Some (media : Media.t) ->
     match media.video, media.video_send with
     | Some Bool false, _ -> false
     | _, None -> true
     | _, Some x -> x

let send_sdp (t : t) : (_RTCSessionDescription Js.t, string) Lwt_result.t =
  let config = t.webrtc_stuff in
  Logs.ign_info "Sending offer/answer SDP...";
  match config.local_sdp, t.webrtc_stuff.pc with
  | None, _ ->
     let s = "Local SDP instance is invalid, not sending anything..." in
     Logs.ign_warning s;
     Lwt_result.fail s
  | _, None ->
     let s = "Peer Connection is invalid, not sending anything..." in
     Logs.ign_warning s;
     Lwt_result.fail s
  | Some _, Some pc ->
     match Js.Opt.to_option pc##.localDescription with
     | None ->
        let s = "localDescription is empty, not sending anything..." in
        Logs.ign_warning s;
        Lwt_result.fail s
     | Some (ld : _RTCSessionDescription Js.t)->
        let (my_sdp : _RTCSessionDescription Js.t) =
          object%js
            val _type = ld##._type
            val sdp = ld##.sdp
          end in
        if not t.webrtc_stuff.trickle
        then (Js.Unsafe.coerce my_sdp)##.trickle := Js._false;
        t.webrtc_stuff <- { t.webrtc_stuff with local_sdp = Some my_sdp
                                              ; sdp_sent = true };
        Lwt_result.return my_sdp

let send_message ?message
      ?(jsep : _RTCSessionDescription Js.t option)
      (t : t)
    : (unit, string) Lwt_result.t =
  (* FIXME check if the session is connected *)
  let (request : Message.t Js.t) =
    Message.make
      ?token:t.token
      ?apisecret:t.apisecret
      ?jsep
      ~transaction:(String.random 12)
      ~janus:"message"
      () in
  Logs.ign_debug_f "Sending message to plugin (handle=%Ld):" t.id;
  Logs.ign_debug ~inspect:request "";
  (* TODO add websockets *)
  Api.http_api_call ~meth:`POST
    (Uri.append_path t.server @@ Printf.sprintf "%Ld/%Ld" t.session_id t.id)
  >|= fun rsp ->
  match Message.parse_response rsp with
  | Ok rsp ->
     Logs.ign_debug "Message sent!";
     Logs.ign_debug ~inspect:rsp "";
     Ok ()
  | Error e -> Error e

let send_trickle_candidate (t : t)
      (candidate : Message.candidate Js.t)
    : (unit, string) Lwt_result.t =
  (* FIXME check the session is connected *)
  let (request : Message.t Js.t) =
    Message.make
      ?token:t.token
      ?apisecret:t.apisecret
      ~candidate:candidate
      ~janus:"trickle"
      ~transaction:(String.random 12)
      () in
  (* TODO add websockets *)
  Api.http_api_call ~meth:`POST
    ~with_credentials:t.with_credentials
    ~body:(Message.to_json request)
    (Uri.append_path t.server @@ Printf.sprintf "%Ld/%Ld" t.session_id t.id)
  >|= fun rsp ->
  match Message.parse_response ~janus_ok:"ack" rsp with
  | Ok rsp ->
     Logs.ign_debug "Candidate sent!";
     Logs.ign_debug ~inspect:rsp "";
     Ok ()
  | Error e -> Error e

(* TODO implement
let send_data
let send_dtmf
 *)

let update_audio_stream ~(replace_audio : bool)
      (stream : mediaStream Js.t)
      (track : mediaStreamTrack Js.t)
      (pc : _RTCPeerConnection Js.t) =
  stream##addTrack track;
  if replace_audio
     && check_browser ~browser:"firefox" ()
     && check_browser ~browser:"chrome" ~ver:72 ~ver_cmp:(>=) ()
  then ()
  else if check_browser ~browser:"firefox" ~ver:59 ~ver_cmp:(>=) ()
  then ()
  else (
    let prefix = if replace_audio then "Replacing" else "Adding" in
    Logs.ign_info_f ~inspect:track "%s audio track:" prefix;
    pc##addTrack track stream)

let update_video_stream ~(replace_video : bool)
      (stream : mediaStream Js.t)
      (track : mediaStreamTrack Js.t)
      (pc : _RTCPeerConnection Js.t) =
  stream##addTrack track;
  if replace_video
     && check_browser ~browser:"firefox" ()
     && check_browser ~browser:"chrome" ~ver:72 ~ver_cmp:(>=) ()
  then (
    Logs.ign_info ~inspect:track "Replacing video track:";
  )
  else if check_browser ~browser:"firefox" ~ver:59 ~ver_cmp:(>=) ()
  then (
    let prefix = if replace_video then "Replacing" else "Adding" in
    Logs.ign_info_f ~inspect:track "%s video track:" prefix;
  )
  else (
    let prefix = if replace_video then "Replacing" else "Adding" in
    Logs.ign_info_f ~inspect:track "%s video track:" prefix;
    pc##addTrack track stream)

let on_ice_conn_state_change (t : t) (_ : #Dom_html.event Js.t) : bool Js.t =
  begin match t.webrtc_stuff.pc with
  | None -> ()
  | Some (pc : _RTCPeerConnection Js.t) ->
     let (state : ice_connection_state) =
       ice_connection_state_of_string
       @@ Js.to_string pc##.iceConnectionState in
     Option.iter (fun f -> f state) t.callbacks.ice_state
  end;
  Js._true

let handle_end_of_candidates (t : t) : (unit, string) Lwt_result.t =
  Logs.ign_info "End of candidates";
  t.webrtc_stuff <- { t.webrtc_stuff with ice_done = true };
  if t.webrtc_stuff.trickle
  then
    (* Notify end of candidates *)
    let candidate = Js.Unsafe.obj [|"completed", Any.of_bool true|] in
    send_trickle_candidate t candidate
  else
    (* No trickle, time to send the complete SDP (including all candidates) *)
    (* FIXME no coercion *)
    Lwt_result.(send_sdp t >|= fun _ -> ())

let on_ice_candidate (t : t)
      (e : _RTCPeerConnectionIceEvent Js.t) : bool Js.t =
  let eoc = Js.string "endOfCandidates" in
  let (t : (unit, string) Lwt_result.t) =
    match get_browser (), Js.Opt.to_option e##.candidate with
    | _, None -> handle_end_of_candidates t
    | "edge", Some c when c##.candidate##indexOf eoc > 0 ->
       handle_end_of_candidates t
    | _, Some (candidate : _RTCIceCandidate Js.t) ->
       (* JSON.stringify doesn't work on some WebRTC objects anymore
				See https://code.google.com/p/chromium/issues/detail?id=467366 *)
       let candidate =
         [| "candidate", Any._of candidate##.candidate
          ; "sdpMid", Any._of candidate##.sdpMid
          ; "sdpMLineIndex", Any._of candidate##.sdpMLineIndex
         |]
         |> Js.Unsafe.obj in
       if t.webrtc_stuff.trickle
       then send_trickle_candidate t candidate
       else Lwt.return_ok () in
  (* XXX what to do with the thread? *)
  Lwt.ignore_result t;
  Js._true

let on_track (t : t) (e : _RTCTrackEvent Js.t) : bool Js.t =
  Logs.ign_info "Handling Remote Track";
  Logs.ign_debug ~inspect:e "";
  match Js.to_array e##.streams with
  | [||] -> Js._true
  | arr ->
     let (stream : mediaStream Js.t) = arr.(0) in
     Option.iter (fun f -> f stream) t.callbacks.on_remote_stream;
     if Js.Optdef.test (Js.Unsafe.coerce e)##.track
        && Js.Optdef.test (Js.Unsafe.coerce e)##.track##.onended
     then (
       Logs.ign_info ~inspect:e##.track "Adding onended callback to track:";
       let onended = fun (e : mediaStreamTrack Dom.event Js.t) ->
         Logs.ign_info ~inspect:e "Remote track removed:";
         match t.webrtc_stuff.remote_stream, Js.Opt.to_option e##.target with
         | None, _ | _, None -> Js._true
         | Some s, Some target ->
            s##removeTrack target;
            Option.iter (fun f -> f s) t.callbacks.on_remote_stream;
            Js._true in
       e##.track##.onended := Dom.handler onended);
     Js._true

let init_pc (t : t) (pc : _RTCPeerConnection Js.t) : unit =
  t.webrtc_stuff <- { t.webrtc_stuff with pc = Some pc };
  Logs.ign_info_f "Preparing local SDP and gathering candidates (trickle=%b)"
    t.webrtc_stuff.trickle;
  pc##.oniceconnectionstatechange := Dom.handler (on_ice_conn_state_change t);
  pc##.onicecandidate := Dom.handler (on_ice_candidate t);
  pc##.ontrack := Dom.handler (on_track t)

let create_pc (t : t) : _RTCPeerConnection Js.t =
  let (pc_config : _RTCConfiguration Js.t) = Js.Unsafe.obj [||] in
  pc_config##.iceServers := Js.array @@ Array.of_list t.ice_servers;
  Option.iter (fun (x : ice_transport_policy) ->
      let v = Js.string @@ ice_transport_policy_to_string x in
      pc_config##.iceTransportPolicy := v) t.ice_transport_policy;
  Option.iter (fun (x : bundle_policy) ->
      let v = Js.string @@ bundle_policy_to_string x in
      pc_config##.bundlePolicy := v) t.bundle_policy;
  (* For Chrome versions before 72, we force a plan-b semantic *)
  if check_browser ~browser:"chrome" ~ver:72 ~ver_cmp:(<) ()
  then (Js.Unsafe.coerce pc_config)##.sdpSemantics := Js.string "plan-b";
  (* If this is Edge, enable BUNDLE explicitly *)
  if check_browser ~browser:"edge" ()
  then (
    let v = Js.string @@ bundle_policy_to_string Max_bundle in
    pc_config##.bundlePolicy := v);
  Logs.ign_info "Creating PeerConnection";
  let (pc_constr : (_RTCConfiguration Js.t ->
                    _RTCPeerConnection Js.t) Js.constr) =
    Js.Unsafe.global##.RTCPeerConnection in
  new%js pc_constr pc_config

let streams_done ?(jsep : _RTCSessionDescription Js.t option)
      ?(stream : mediaStream Js.t option)
      (media : Media.t)
      (t : t) =
  let config = t.webrtc_stuff in
  (* If we still need to create a PeerConnection, let's do that *)
  let (pc : _RTCPeerConnection Js.t) = match t.webrtc_stuff.pc with
    | Some pc -> pc
    | None ->
       let pc = create_pc t in
       init_pc t pc;
       pc in
  begin match config.local_stream, media.update, config.stream_external with
  | None, _, _ | _, false, _ | _, _, true ->
     t.webrtc_stuff <- { config with local_stream = stream };
     begin match stream with
     | None -> ()
     | Some stream ->
        Logs.ign_info "Adding local stream";
        let tracks = stream##getTracks in
        let cb = fun (track : mediaStreamTrack Js.t) _ _ ->
          Logs.ign_info ~inspect:track "Adding local track:";
          pc##addTrack track stream in
        tracks##forEach (Js.wrap_callback cb)
     end
  | Some my_stream, _, _ ->
     (* We only need to update the existing stream *)
     let (audio_track : mediaStreamTrack Js.t option) = match stream with
       | None -> None
       | Some s -> array_get s##getAudioTracks 0 in
     begin match audio_track with
     | None -> ()
     | Some track ->
        let ({ update; add_audio; replace_audio; _ } : Media.t) = media in
        if ((not update && is_audio_send_enabled ~media ())
            || (update && (add_audio || replace_audio)))
        then update_audio_stream ~replace_audio my_stream track pc
     end;
     let (video_track : mediaStreamTrack Js.t option) = match stream with
       | None -> None
       | Some s -> array_get s##getVideoTracks 0 in
     begin match video_track with
     | None -> ()
     | Some track ->
        let ({ update; add_video; replace_video; _ } : Media.t) = media in
        if ((not update && is_video_send_enabled ~media ())
            || (update && (add_video || replace_video)))
        then update_video_stream ~replace_video my_stream track pc
     end;
  end;
  (* Any data channel to create? *)
  begin match is_data_enabled ~media (), t.webrtc_stuff.pc with
  | true, Some pc ->
     Logs.ign_info "Creating data channel";
     let on_message = fun e ->
       Logs.ign_info ~inspect:e##.data "Received message on data channel:";
       Option.iter (fun f -> f @@ Js.to_string e##.data) t.callbacks.on_data;
       Js._true in
     let on_state_change = fun _ ->
       let state = match t.webrtc_stuff.data_channel with
         | None -> "null"
         | Some dc -> Js.to_string dc##.readyState in
       if String.equal state "open"
       then Option.iter (fun f -> f ()) t.callbacks.on_data_open;
       Js._true in
     let on_error = fun e ->
       Logs.ign_error ~inspect:e "Got error on data channel:";
       Option.iter (fun f -> f e) t.callbacks.on_data_error;
       Js._true in
     let dc = pc##createDataChannel in
     dc##.onmessage := Dom.handler on_message;
     dc##.onopen := Dom.handler on_state_change;
     dc##.onclose := Dom.handler on_state_change;
     dc##.onerror := Dom.handler on_error
  | _ -> ()
  end;
  (* If there is a new local stream, let's notify the application *)
  begin match t.webrtc_stuff.local_stream with
  | None -> ()
  | Some (stream : mediaStream Js.t) ->
     Option.iter (fun f -> f stream) t.callbacks.on_local_stream;
  end;
  (* Create offer/answer now *)
  begin match jsep, t.webrtc_stuff.pc with
  | None, _ | _, None -> ()
  | Some jsep, Some pc ->
     pc##setRemoteDescription jsep
  end

let id (t : t) : int64 =
  t.id

let plugin (t : t) : string =
  t.plugin

let volume (t : t) : unit =
  ignore t

let audio_muted (t : t) : bool =
  ignore t;
  false

let mute_audio (t : t) : unit =
  ignore t

let unmute_audio (t : t) : unit =
  ignore t

let video_muted (t : t) : bool =
  ignore t;
  false

let mute_video (t : t) : unit =
  ignore t

let unmute_video (t : t) : unit =
  ignore t

let bitrate (t : t) : unit =
  ignore t

let send (t : t) : unit =
  ignore t

let send_data (t : t) (text : string) : unit =
  Logs.ign_info_f "Sending string on data channel: %s" text;
  ignore t

let create_offer (t : t) : unit =
  ignore t

let create_answer (t : t) : unit =
  ignore t

let handle_remote_jsep (t : t) : unit =
  ignore t

let dtmf (t : t) : unit =
  ignore t

let hangup (t : t) : unit =
  ignore t

let detach (t : t) : unit =
  ignore t

