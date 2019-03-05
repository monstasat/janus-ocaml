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
    ; candidates : _RTCIceCandidateInit Js.t Js.js_array Js.t
    ; stream_external : bool
    ; remote_stream : mediaStream Js.t option
    ; local_sdp : _RTCSessionDescription Js.t option
    ; remote_sdp : _RTCSessionDescription Js.t option
    ; media_constraints : _RTCOfferOptions Js.t option
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
    ; candidates = new%js Js.array_empty
    ; local_stream = None
    ; stream_external = false
    ; remote_stream = None
    ; local_sdp = None
    ; remote_sdp = None
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
  ; mutable webrtc : Webrtc_stuff.t
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

let is_audio_recv_enabled ?(media : Media.t option) () : bool =
  match media with
  | None -> true (* Default *)
  | Some (media : Media.t) ->
     match media.audio, media.audio_recv with
     | Some Bool false, _ -> false (* Generic audio has precedence *)
     | _, None -> true (* default *)
     | _, Some x -> x

let is_video_send_enabled ?(media : Media.t option) () : bool =
  match media with
  | None -> true
  | Some (media : Media.t) ->
     match media.video, media.video_send with
     | Some Bool false, _ -> false
     | _, None -> true
     | _, Some x -> x

let is_video_recv_enabled ?(media : Media.t option) () : bool =
  match media with
  | None -> true (* Default *)
  | Some (media : Media.t) ->
     match media.video, media.video_recv with
     | Some Bool false, _ -> false (* Generic video has precedence *)
     | _, None -> true (* default *)
     | _, Some x -> x

let munge_sdp_for_simulcasting (sdp : Js.js_string Js.t)
    : Js.js_string Js.t =
  (* FIXME implement *)
  sdp

let exn_to_string : exn -> string = function
  | Js.Error e -> Js.to_string e##toString
  | Failure s -> s
  | exn -> Printexc.to_string exn

let send_sdp (t : t) : (_RTCSessionDescription Js.t, string) Lwt_result.t =
  Log.ign_info "Sending offer/answer SDP...";
  match t.webrtc.local_sdp, t.webrtc.pc with
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
        if not t.webrtc.trickle
        then (Js.Unsafe.coerce my_sdp)##.trickle := Js._false;
        t.webrtc <- { t.webrtc with local_sdp = Some my_sdp
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
  then (
    Log.ign_info ~inspect:track "Replacing audio track:";
    let rec aux = function
      | [] -> ()
      | (sender : _RTCRtpSender Js.t) :: tl ->
         (match Js.Opt.to_option sender##.track with
          | None -> ()
          | Some (track' : mediaStreamTrack Js.t) ->
             if String.equal "audio" (Js.to_string track'##.kind)
             then ignore @@ sender##replaceTrack (Js.some track));
         aux tl in
    aux (Array.to_list @@ Js.to_array @@ pc##getSenders)
  )
  else if check_browser ~browser:"firefox" ~ver:59 ~ver_cmp:(>=) ()
  then (
  (* Firefox >= 59 uses Transceivers *)
  (* TODO implement *)
  )
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
    let rec aux = function
      | [] -> ()
      | (sender : _RTCRtpSender Js.t) :: tl ->
         (match Js.Opt.to_option sender##.track with
          | None -> ()
          | Some (track' : mediaStreamTrack Js.t) ->
             if String.equal "video" (Js.to_string track'##.kind)
             then ignore @@ sender##replaceTrack (Js.some track));
         aux tl in
    aux (Array.to_list @@ Js.to_array @@ pc##getSenders)
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
  begin match t.webrtc.pc with
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
  t.webrtc <- { t.webrtc with ice_done = true };
  if t.webrtc.trickle
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
       if t.webrtc.trickle
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
         match t.webrtc.remote_stream, Js.Opt.to_option e##.target with
         | None, _ | _, None -> Js._true
         | Some s, Some target ->
            s##removeTrack target;
            Option.iter (fun f -> f s) t.on_remote_stream;
            Js._true in
       e##.track##.onended := Dom.handler onended);
     Js._true

let init_pc (t : t) (pc : _RTCPeerConnection Js.t) : unit =
  Log.ign_info_f "Preparing local SDP and gathering candidates (trickle=%b)"
    t.webrtc.trickle;
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
    let state = match t.webrtc.data_channel with
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
  let (pc : _RTCPeerConnection Js.t) = match t.webrtc.pc with
    | Some pc -> pc
    | None ->
       let pc = create_pc t in
       t.webrtc <- { t.webrtc with pc = Some pc };
       init_pc t pc;
       pc in
  begin match t.webrtc.local_stream,
              media.update,
              t.webrtc.stream_external with
  | None, _, _ | _, false, _ | _, _, true ->
     t.webrtc <- { t.webrtc with local_stream = stream };
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
    t.webrtc <- { t.webrtc with data_channel = Some dc};
    init_data_channel t dc);
  (* If there is a new local stream, let's notify the application *)
  begin match t.webrtc.local_stream with
  | None -> ()
  | Some (stream : mediaStream Js.t) ->
     Option.iter (fun f -> f stream) t.on_local_stream;
  end;
  (* Create offer/answer now *)
  begin match jsep with
  | None -> ()
  | Some jsep ->
     let on_ok = fun () ->
       Log.ign_info "Remote description accepted!";
       t.webrtc <- { t.webrtc with remote_sdp = Some jsep } in
     Promise.Infix.(
      pc##setRemoteDescription jsep
      >|| (on_ok, (fun _ -> ())))
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

let handle_transceiver ~(recv_en : bool) ~(send_en : bool) ~(need_rm : bool)
      (transceiver : _RTCRtpTransceiver Js.t option)
      (pc : _RTCPeerConnection Js.t)
      (typ : [`Audio | `Video]) =
  let kind = match typ with
    | `Audio -> "audio"
    | `Video -> "video" in
  match send_en, recv_en, transceiver with
  | false, false, Some (tr : _RTCRtpTransceiver Js.t) ->
     (* Track disabled: have we removed it? *)
     if need_rm then (
       tr##.direction := Js.string "inactive";
       Log.ign_info_f ~inspect:tr "Setting %s transceiver to inactive:" kind)
  | true, true, Some (tr : _RTCRtpTransceiver Js.t) ->
     tr##.direction := Js.string "sendrecv";
     Log.ign_info_f ~inspect:tr "Setting %s transceiver to sendrecv:" kind
  | true, false, Some (tr : _RTCRtpTransceiver Js.t) ->
     tr##.direction := Js.string "sendonly";
     Log.ign_info_f ~inspect:tr "Setting %s transceiver to sendonly:" kind
  | false, true, Some (tr : _RTCRtpTransceiver Js.t) ->
     tr##.direction := Js.string "recvonly";
     Log.ign_info_f ~inspect:tr "Setting %s transceiver to recvonly:" kind
  | false, true, None ->
     (* In theory, this is the only case where we might
        not have a transceiver yet *)
     let (init : _RTCRtpTransceiverInit Js.t) = Js.Unsafe.obj [||] in
     init##.direction := Js.string "recvonly";
     let tr = pc##addTransceiver' (Js.string kind) init in
     Log.ign_info_f ~inspect:tr "Adding recvonly %s transceiver:" kind
  | _ -> ()

let create_offer ?(ice_restart = false) ?(simulcast = false)
      (media : Media.t) (t : t)
    : (_RTCSessionDescription Js.t option, string) Lwt_result.t =
  t.webrtc.pc
  |> Option.to_result_lazy (fun () ->
         "create_offer: RTCPeerConnection is not established")
  |> Lwt_result.lift
  >>= fun (pc : _RTCPeerConnection Js.t) ->
  Log.ign_info_f "Creating offer (iceDone=%b, simulcast=%b)"
    t.webrtc.ice_done simulcast;
  let (media_constraints : _RTCOfferOptions Js.t) = Js.Unsafe.obj [||] in
  let audio_send = is_audio_send_enabled ~media () in
  let audio_recv = is_audio_recv_enabled ~media () in
  let video_send = is_video_send_enabled ~media () in
  let video_recv = is_video_recv_enabled ~media () in
  if check_browser ~browser:"firefox" ~ver:59 ~ver_cmp:(>=) ()
  then (
    (* Firefox >= 59 uses Transceivers *)
    let transceivers = pc##getTransceivers in
    let audio_transceiver, video_transceiver =
      if transceivers##.length <= 0 then None, None else
        let cb = fun (atr', vtr') (tr : _RTCRtpTransceiver Js.t) _ _ ->
          let strack = Js.Opt.to_option tr##.sender##.track in
          let rtrack = Js.Opt.to_option tr##.receiver##.track in
          let atr = match atr', strack, rtrack with
            | None, Some s, Some r ->
               if String.equal "audio" (Js.to_string s##.kind)
                  && String.equal "audio" (Js.to_string r##.kind)
               then Some tr else None
            | _ -> None in
          let vtr = match atr, vtr', strack, rtrack with
            | None, None, Some s, Some r ->
               if String.equal "video" (Js.to_string s##.kind)
                  && String.equal "video" (Js.to_string r##.kind)
               then Some tr else None
            | _ -> None in
          atr, vtr in
        transceivers##reduce_init (Js.wrap_callback cb) (None, None) in
    (* Handle audio (and related changes, if any) *)
    handle_transceiver
      ~recv_en:audio_recv
      ~send_en:audio_send
      ~need_rm:media.remove_audio
      audio_transceiver pc `Audio;
    (* Handle video (and related changes, if any) *)
    handle_transceiver
      ~recv_en:video_recv
      ~send_en:video_send
      ~need_rm:media.remove_video
      video_transceiver pc `Video)
  else (
    media_constraints##.offerToReceiveAudio := Js.bool audio_recv;
    media_constraints##.offerToReceiveVideo := Js.bool video_recv;
  );
  if ice_restart then media_constraints##.iceRestart := Js._true;
  Log.ign_debug ~inspect:media_constraints "";
  (* Check if this is Firefox and we've been asked to do simulcasting *)
  if video_send && simulcast && check_browser ~browser:"firefox" ()
  then (
    Log.ign_info "Enabling simulcasting for Firefox (RID)";
    let cb = fun (s : _RTCRtpSender Js.t) _ _ ->
      match Js.Opt.to_option s##.track with
      | None -> Js._false
      | Some t -> Js.bool @@ String.equal (Js.to_string t##.kind) "video" in
    let (sender : _RTCRtpSender Js.t option) =
      (Js.Unsafe.coerce pc##getSenders)##find (Js.wrap_callback cb)
      |> Js.Optdef.to_option in
    match sender with
    | None -> ()
    | Some (sender : _RTCRtpSender Js.t) ->
       let make_enc priority bitrate : _RTCRtpEncodingParameters Js.t =
         [| "rid", Js.Unsafe.inject @@ Js.string priority
          ; "active", Js.Unsafe.inject @@ Js._true
          ; "priority", Js.Unsafe.inject @@ Js.string priority
          ; "maxBitrate", Js.Unsafe.inject bitrate
         |]
         |> Js.Unsafe.obj in
       let (parameters : _RTCRtpParameters Js.t) = sender##getParameters in
       let (encodings : _RTCRtpEncodingParameters Js.t Js.js_array Js.t) =
         [| make_enc "high" 1_000_000
          ; make_enc "medium" 300_000
          ; make_enc "low" 100_000
         |]
         |> Js.array in
       parameters##.encodings := encodings;
       sender##setParameters parameters);
  Lwt.try_bind (fun () ->
      Promise.to_lwt @@ pc##createOffer' media_constraints)
    (fun (offer : _RTCSessionDescription Js.t) ->
      Log.ign_debug ~inspect:offer "";
      Log.ign_info "Setting local description";
      if video_send && simulcast
      then (
        (* This SDP munging only works with chrome
           (Safari STP may support it too) *)
        if check_browser ~browser:"chrome" ()
           || check_browser ~browser:"safari" ()
        then (
          Log.ign_info "Enabling Simulcasting for Chrome (SDP munging)";
          let new_sdp = munge_sdp_for_simulcasting offer##.sdp in
          (Js.Unsafe.coerce offer)##.sdp := new_sdp)
        else if not (check_browser ~browser:"firefox" ())
        then Log.ign_warning "simulcast=true, but this is not Chrome \
                              nor Firefox, ignoring");
      t.webrtc <- { t.webrtc with local_sdp = Some offer
                                ; media_constraints = Some media_constraints };
      Lwt.try_bind (fun () -> Promise.to_lwt @@ pc##setLocalDescription offer)
        (fun () ->
          if not t.webrtc.ice_done && not t.webrtc.trickle
          then (
            Log.ign_info "Waiting for all candidates...";
            (* XXX original code do not call success callback here *)
            Lwt.return_ok None)
          else (
            Log.ign_info "Offer ready";
            (* JSON.stringify doesn't work on some WebRTC objects anymore
				       See https://code.google.com/p/chromium/issues/detail?id=467366 *)
            let (jsep : _RTCSessionDescription Js.t) =
              object%js
                val _type = offer##._type
                val sdp = offer##.sdp
              end in
            Lwt.return_ok (Some jsep)))
        (fun e -> Lwt.return_error @@ exn_to_string e))
    (fun e -> Lwt.return_error @@ exn_to_string e)

let create_answer (t : t) : unit =
  ignore t

let handle_remote_jsep (jsep : _RTCSessionDescription Js.t)
      (t : t) : (unit, string) Lwt_result.t =
  match t.webrtc.pc with
  | None ->
     let s =
       "No PeerConnection: \
        if this is an answer, use create_answer \
        and not handle_remote_jsep" in
     Log.ign_warning s;
     Lwt.return_error s;
  | Some (pc : _RTCPeerConnection Js.t) ->
     Lwt.try_bind (fun () ->
         Promise.to_lwt @@ pc##setRemoteDescription jsep)
       (fun () ->
         Log.ign_info_f "Remote description accepted";
         t.webrtc <- { t.webrtc with remote_sdp = Some jsep };
         if t.webrtc.candidates##.length > 0 then (
           (* Any trickle candidate we cached? *)
           let cb = fun (c : _RTCIceCandidateInit Js.t) _ _ ->
             Log.ign_debug ~inspect:c "Adding remote candidate:";
             let (completed : bool) =
               Js.Optdef.get (Js.Unsafe.coerce c)##.completed
                 (fun () -> Js._false)
               |> Js.to_bool in
             if completed
             then ignore @@ pc##addIceCandidate (Js.Unsafe.obj [||])
             else ignore @@ pc##addIceCandidate c in
           t.webrtc.candidates##forEach (Js.wrap_callback cb);
           t.webrtc.candidates##.length := 0);
         Lwt.return_ok ())
       (function
        | Js.Error e -> Lwt.return_error (Js.to_string e##toString)
        | Failure s -> Lwt.return_error s
        | e -> Lwt.return_error @@ Printexc.to_string e)

let dtmf (t : t) : unit =
  ignore t

let hangup (t : t) : unit =
  ignore t

let detach (t : t) : unit =
  ignore t

