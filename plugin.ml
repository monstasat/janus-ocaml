open Js_of_ocaml
open Utils
open Webrtc
open Adapter
open Lwt_result.Infix

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
  | Custom of string

let typ_to_string : typ -> string = function
  | Audiobridge -> "janus.plugin.audiobridge"
  | Echotest -> "janus.plugin.echotest"
  | Recordplay -> "janus.plugin.recordplay"
  | Sip -> "janus.plugin.sip"
  | Streaming -> "janus.plugin.streaming"
  | Textroom -> "janus.plugin.textroom"
  | Videocall -> "janus.plugin.videocall"
  | Videoroom -> "janus.plugin.videoroom"
  | Voicemail -> "janus.plugin.voicemail"
  | Custom s -> s

type ice_transport_policy =
  | All
  | Public
  | Relay

let ice_transport_policy_to_string : ice_transport_policy -> string = function
  | All -> "all"
  | Public -> "public"
  | Relay -> "relay"

type bundle_policy =
  | Balanced
  | Max_compact
  | Max_bundle

let bundle_policy_to_string : bundle_policy -> string = function
  | Balanced -> "balanced"
  | Max_compact -> "max-compact"
  | Max_bundle -> "max-bundle"

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

type webrtc_state =
  | Up
  | Down of string

type media_state =
  | Video of bool
  | Audio of bool

type slow_link =
  { nacks : int
  ; uplink : bool
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
    ; data_channel : _RTCDataChannel Js.t option
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

  type data =
    | Bool of bool
    | Options of data_options
  and data_options =
    { ordered : bool option
    ; max_packet_life_time : int option
    ; max_retransmits : int option
    ; protocol : string option
    ; negotiated : bool option
    ; id : int option
    }

  type t =
    { audio_send : bool option
    ; audio_recv : bool option
    ; audio : audio option
    ; video_send : bool option
    ; video_recv : bool option
    ; video : video option
    ; data : data option
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
  { id : int
  ; opaque_id : string option
  ; server : string
  ; session_id : int
  ; is_connected : unit -> bool
  ; plugin : typ
  ; token : string option
  ; apisecret : string option
  ; with_credentials : bool
  ; ice_servers : _RTCIceServer Js.t list
  ; ice_transport_policy : ice_transport_policy option
  ; bundle_policy : bundle_policy option
  ; mutable webrtc_stuff : Webrtc_stuff.t
  ; mutable detached : bool
  (* Callbacks *)
  ; on_local_stream : (mediaStream Js.t -> unit) option
  ; on_remote_stream : (mediaStream Js.t -> unit) option
  ; on_message : 'a. (?jsep:_RTCSessionDescription Js.t -> 'a Js.t -> unit) option
  ; on_jsep : (unit -> unit) option
  ; on_consent_dialog : (bool -> unit) option
  ; on_ice_state : (ice_connection_state -> unit) option
  ; on_webrtc_state : (webrtc_state -> unit) option
  ; on_media_state : (media_state -> unit) option
  ; on_slow_link : (slow_link -> unit) option
  ; on_data : (string -> unit) option (* FIXME check type *)
  ; on_data_open : (unit -> unit) option
  ; on_data_error : (< > Js.t -> unit) option (* FIXME add type *)
  ; on_cleanup : (unit -> unit) option
  ; on_detached : int -> unit
  }

let is_data_enabled ?(media : Media.t option) () : bool =
  match get_browser () with
  | "edge" ->
     Log.ign_warning "Edge doen't support data channels yet";
     false
  | _ ->
     match media with
     | None | Some { data = None; _ } -> false (* Default *)
     | Some { data = Some Bool x; _ } -> x
     | Some { data = Some Options _; _ } -> true

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
  Log.ign_info "Sending offer/answer SDP...";
  match config.local_sdp, t.webrtc_stuff.pc with
  | None, _ ->
     let s = "Local SDP instance is invalid, not sending anything..." in
     Log.ign_warning s;
     Lwt_result.fail s
  | _, None ->
     let s = "Peer Connection is invalid, not sending anything..." in
     Log.ign_warning s;
     Lwt_result.fail s
  | Some _, Some pc ->
     match Js.Opt.to_option pc##.localDescription with
     | None ->
        let s = "localDescription is empty, not sending anything..." in
        Log.ign_warning s;
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

let send_message ?(message : 'a Js.t option)
      ?(jsep : _RTCSessionDescription Js.t option)
      (t : t)
    : ('a Js.t option, string) Lwt_result.t =
  is_connected_lwt t.is_connected
  >>= fun () ->
  let (request : Api.Msg.req Js.t) =
    Api.Msg.make_req
      ?token:t.token
      ?apisecret:t.apisecret
      ?body:message
      ?jsep
      ~transaction:(String.random 12)
      ~janus:"message"
      () in
  Log.ign_debug_f "Sending message to plugin (handle=%d):" t.id;
  Log.ign_debug ~inspect:request "";
  (* TODO add websockets *)
  Lwt.Infix.(
    Api.http_call ~meth:`POST ~body:request
      (Printf.sprintf "%s/%d/%d" t.server t.session_id t.id)
    >|= function
    | Error e -> Error (Api.error_to_string e)
    | Ok rsp ->
       Log.ign_debug "Message sent!";
       Log.ign_debug ~inspect:rsp "";
       let f (m : Api.Msg.msg Js.t) = function
         | "ack" -> Some `Ack
         | "success" ->
            let (x : Api.Msg.handle_event Js.t) = Js.Unsafe.coerce m in
            Some (`Data (Js.Optdef.to_option x##.plugindata))
         | _ -> None in
       match Api.Msg.check_msg_map f rsp with
       | Error e -> Error e
       | Ok `Ack ->
          (* The plugin decided to handle the request asynchronously *)
          Ok None
       | Ok `Data None ->
          (* We got a success, must be a synchronous transaction *)
          Log.ign_warning "Request succeeded, but missing plugindata...";
          Ok None
       | Ok `Data Some d ->
          (* We got a success, must be a synchronous transaction *)
          let plugin = Js.to_string d##.plugin in
          Log.ign_info_f "Synchronous transaction successful (%s)" plugin;
          Log.ign_debug ~inspect:d##.data "";
          Ok (Some d##.data))

let send_trickle_candidate (t : t)
      (candidate : Api.Msg.candidate Js.t)
    : (unit, string) Lwt_result.t =
  is_connected_lwt t.is_connected
  >>= fun () ->
  let (request : Api.Msg.req Js.t) =
    Api.Msg.make_req
      ?token:t.token
      ?apisecret:t.apisecret
      ~candidate:candidate
      ~janus:"trickle"
      ~transaction:(String.random 12)
      () in
  (* TODO add websockets *)
  Lwt.Infix.(
    Api.http_call ~meth:`POST
      ~with_credentials:t.with_credentials
      ~body:request
      (Printf.sprintf "%s/%d/%d" t.server t.session_id t.id)
    >|= function
    | Error e -> Error (Api.error_to_string e)
    | Ok rsp ->
       match Api.Msg.check_msg ~key:"ack" rsp with
       | Error e -> Error e
       | Ok rsp ->
          Log.ign_debug "Candidate sent!";
          Log.ign_debug ~inspect:rsp "";
          Ok ())

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
    Log.ign_info_f ~inspect:track "%s audio track:" prefix;
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
    Log.ign_info ~inspect:track "Replacing video track:";
  )
  else if check_browser ~browser:"firefox" ~ver:59 ~ver_cmp:(>=) ()
  then (
    let prefix = if replace_video then "Replacing" else "Adding" in
    Log.ign_info_f ~inspect:track "%s video track:" prefix;
  )
  else (
    let prefix = if replace_video then "Replacing" else "Adding" in
    Log.ign_info_f ~inspect:track "%s video track:" prefix;
    pc##addTrack track stream)

let on_ice_conn_state_change (t : t) (_ : #Dom_html.event Js.t) : bool Js.t =
  begin match t.webrtc_stuff.pc with
  | None -> ()
  | Some (pc : _RTCPeerConnection Js.t) ->
     let (state : ice_connection_state) =
       ice_connection_state_of_string
       @@ Js.to_string pc##.iceConnectionState in
     Option.iter (fun f -> f state) t.on_ice_state
  end;
  Js._true

let handle_end_of_candidates (t : t) : (unit, string) Lwt_result.t =
  Log.ign_info "End of candidates";
  t.webrtc_stuff <- { t.webrtc_stuff with ice_done = true };
  if t.webrtc_stuff.trickle
  then
    (* Notify end of candidates *)
    let (candidate : Api.Msg.candidate Js.t) = Js.Unsafe.obj [||] in
    candidate##.completed := Js._true;
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
    | _, Some (c : _RTCIceCandidate Js.t) ->
       (* JSON.stringify doesn't work on some WebRTC objects anymore
				See https://code.google.com/p/chromium/issues/detail?id=467366 *)
       let (candidate : Api.Msg.candidate Js.t) = Js.Unsafe.obj [||] in
       candidate##.candidate := c##.candidate;
       candidate##.sdpMid := c##.sdpMid;
       candidate##.sdpMLineIndex := c##.sdpMLineIndex;
       if t.webrtc_stuff.trickle
       then send_trickle_candidate t candidate
       else Lwt.return_ok () in
  (* XXX what to do with the thread? *)
  Lwt.ignore_result t;
  Js._true

let on_track (t : t) (e : _RTCTrackEvent Js.t) : bool Js.t =
  Log.ign_info "Handling Remote Track";
  Log.ign_debug ~inspect:e "";
  match Js.to_array e##.streams with
  | [||] -> Js._true
  | arr ->
     let (stream : mediaStream Js.t) = arr.(0) in
     Option.iter (fun f -> f stream) t.on_remote_stream;
     if Js.Optdef.test (Js.Unsafe.coerce e)##.track
        && Js.Optdef.test (Js.Unsafe.coerce e)##.track##.onended
     then (
       Log.ign_info ~inspect:e##.track "Adding onended callback to track:";
       let onended = fun (e : mediaStreamTrack Dom.event Js.t) ->
         Log.ign_info ~inspect:e "Remote track removed:";
         match t.webrtc_stuff.remote_stream, Js.Opt.to_option e##.target with
         | None, _ | _, None -> Js._true
         | Some s, Some target ->
            s##removeTrack target;
            Option.iter (fun f -> f s) t.on_remote_stream;
            Js._true in
       e##.track##.onended := Dom.handler onended);
     Js._true

let init_pc (t : t) (pc : _RTCPeerConnection Js.t) : unit =
  Log.ign_info_f "Preparing local SDP and gathering candidates (trickle=%b)"
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
  Log.ign_info "Creating PeerConnection";
  let (pc_constr : (_RTCConfiguration Js.t ->
                    _RTCPeerConnection Js.t) Js.constr) =
    Js.Unsafe.global##.RTCPeerConnection in
  new%js pc_constr pc_config

let init_data_channel (t : t) (dc : _RTCDataChannel Js.t) : unit =
  let on_message = fun e ->
    Log.ign_info ~inspect:e##.data "Received message on data channel:";
    Option.iter (fun f -> f @@ Js.to_string e##.data) t.on_data;
    Js._true in
  let on_state_change = fun _ ->
    let state = match t.webrtc_stuff.data_channel with
      | None -> "null"
      | Some dc -> Js.to_string dc##.readyState in
    (* FIXME change callback to on_data_state_change *)
    if String.equal state "open"
    then Option.iter (fun f -> f ()) t.on_data_open;
    Js._true in
  let on_error = fun e ->
    Log.ign_error ~inspect:e "Got error on data channel:";
    Option.iter (fun f -> f @@ Js.Unsafe.coerce e) t.on_data_error;
    Js._true in
  (* FIXME add event handlers to type *)
  let dc = Js.Unsafe.coerce dc in
  dc##.onmessage := Dom.handler on_message;
  dc##.onopen := Dom.handler on_state_change;
  dc##.onclose := Dom.handler on_state_change;
  dc##.onerror := Dom.handler on_error

let create_data_channel ~(media : Media.t)
      (pc : _RTCPeerConnection Js.t) : _RTCDataChannel Js.t =
  Log.ign_info "Creating data channel"; let options = match media.data with
    | None | Some Bool _ ->
       (* Create default options *)
       let (opts : _RTCDataChannelInit Js.t) = Js.Unsafe.obj [||] in
       opts##.ordered := Js._false;
       opts
    | Some Options { ordered
                   ; max_packet_life_time = lt
                   ; max_retransmits
                   ; protocol
                   ; negotiated
                   ; id } ->
       let (opts : _RTCDataChannelInit Js.t) = Js.Unsafe.obj [||] in
       Option.(
         iter (fun x -> opts##.ordered := Js.bool x) ordered;
         iter (fun x -> opts##.maxPacketLifeTime := Js.some x) lt;
         iter (fun x -> opts##.maxRetransmits := Js.some x) max_retransmits;
         iter (fun x -> opts##.protocol := Js.string x) protocol;
         iter (fun x -> opts##.negotiated := Js.bool x) negotiated;
         iter (fun x -> opts##.id := x) id);
       opts in
  pc##createDataChannel_init (Js.string "JanusDataChannel") options

let streams_done ?(jsep : _RTCSessionDescription Js.t option)
      ?(stream : mediaStream Js.t option)
      (media : Media.t)
      (t : t) =
  (* If we still need to create a PeerConnection, let's do that *)
  let (pc : _RTCPeerConnection Js.t) = match t.webrtc_stuff.pc with
    | Some pc -> pc
    | None ->
       let pc = create_pc t in
       t.webrtc_stuff <- { t.webrtc_stuff with pc = Some pc };
       init_pc t pc;
       pc in
  begin match t.webrtc_stuff.local_stream,
              media.update,
              t.webrtc_stuff.stream_external with
  | None, _, _ | _, false, _ | _, _, true ->
     t.webrtc_stuff <- { t.webrtc_stuff with local_stream = stream };
     begin match stream with
     | None -> ()
     | Some stream ->
        Log.ign_info "Adding local stream";
        let tracks = stream##getTracks in
        let cb = fun (track : mediaStreamTrack Js.t) _ _ ->
          Log.ign_info ~inspect:track "Adding local track:";
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
  if is_data_enabled ~media ()
  then (
    let dc = create_data_channel ~media pc in
    t.webrtc_stuff <- { t.webrtc_stuff with data_channel = Some dc};
    init_data_channel t dc);
  (* If there is a new local stream, let's notify the application *)
  begin match t.webrtc_stuff.local_stream with
  | None -> ()
  | Some (stream : mediaStream Js.t) ->
     Option.iter (fun f -> f stream) t.on_local_stream;
  end;
  (* Create offer/answer now *)
  begin match jsep with
  | None -> ()
  | Some jsep -> pc##setRemoteDescription jsep
  end

let id (t : t) : int =
  t.id

let typ (t : t) : typ =
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

let send_data (t : t) (text : string) : unit =
  Log.ign_info_f "Sending string on data channel: %s" text;
  ignore t

let create_offer (t : t) : unit =
  ignore t

let create_answer (t : t) : unit =
  ignore t

let handle_remote_jsep (jsep : _RTCSessionDescription Js.t)
      (t : t) : (unit, string) Lwt_result.t =
  match t.webrtc_stuff.pc with
  | None ->
     let s =
       "No PeerConnection: \
        if this is an answer, use create_answer \
        and not handle_remote_jsep" in
     Log.ign_warning s;
     Lwt.return_error s;
  | Some (pc : _RTCPeerConnection Js.t) ->
     (* FIXME handle promise *)
     pc##setRemoteDescription jsep;
     Lwt.return_ok ()

let dtmf (t : t) : unit =
  ignore t

let hangup (t : t) : unit =
  ignore t

let detach (t : t) : unit =
  ignore t

