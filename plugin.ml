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
  | Sip -> "janus.plugin.sip"
  | Textroom -> "janus.plugin.textroom"
  | Echotest -> "janus.plugin.echotest"
  | Streaming -> "janus.plugin.streaming"
  | Videocall -> "janus.plugin.videocall"
  | Videoroom -> "janus.plugin.videoroom"
  | Voicemail -> "janus.plugin.voicemail"
  | Recordplay -> "janus.plugin.recordplay"
  | Audiobridge -> "janus.plugin.audiobridge"
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
    ; local_sdp : _RTCSessionDescriptionInit Js.t option
    ; remote_sdp : _RTCSessionDescriptionInit Js.t option
    ; pc : _RTCPeerConnection Js.t option
    ; data_channel : _RTCDataChannel Js.t option
    ; dtmf_sender : unit option
    ; trickle : bool
    ; ice_done : bool
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
    ; pc = None
    ; data_channel = None
    ; dtmf_sender = None
    ; trickle = true
    ; ice_done = false
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

type t =
  { id : int
  ; opaque_id : string option
  ; server : string
  ; session_id : int
  ; is_connected : unit -> bool
  ; plugin : typ
  ; token : string option
  ; apisecret : string option
  ; ipv6 : bool
  ; with_credentials : bool
  ; rtc_constraints : (string * Js.Unsafe.any) array list
  ; ice_servers : _RTCIceServer Js.t list
  ; ice_transport_policy : ice_transport_policy option
  ; bundle_policy : bundle_policy option
  ; mutable webrtc : Webrtc_stuff.t
  ; mutable detached : bool
  (* Callbacks *)
  ; on_local_stream : (mediaStream Js.t -> unit) option
  ; on_remote_stream : (mediaStream Js.t -> unit) option
  ; on_message : 'a. (?jsep:_RTCSessionDescriptionInit Js.t ->
                      'a Js.t ->
                      t ->
                      unit) option
  ; on_consent_dialog : (bool -> unit) option
  ; on_ice_state : (ice_connection_state -> unit) option
  ; on_webrtc_state : (webrtc_state -> unit) option
  ; on_media_state : (media_state -> unit) option
  ; on_slow_link : (slow_link -> unit) option
  ; on_data : (string -> unit) option (* FIXME check type *)
  ; on_data_open : (unit -> unit) option
  ; on_data_error : (< > Js.t -> unit) option (* FIXME add type *)
  ; on_cleanup : (unit -> unit) option
  ; on_detached : (unit -> unit) option
  ; rm_from_session : int -> unit
  }

module Peer_connection = struct

  let send_sdp (t : t) :
        (_RTCSessionDescriptionInit Js.t, string) Lwt_result.t =
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
          let (my_sdp : _RTCSessionDescriptionInit Js.t) = Js.Unsafe.obj [||] in
          my_sdp##._type := ld##._type;
          my_sdp##.sdp := ld##.sdp;
          if not t.webrtc.trickle
          then (Js.Unsafe.coerce my_sdp)##.trickle := Js._false;
          t.webrtc <- { t.webrtc with local_sdp = Some my_sdp };
          Lwt_result.return my_sdp

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
    Log.ign_debug ~inspect:e "";
    let (t : (unit, string) Lwt_result.t) =
      match get_browser (), Js.Opt.to_option e##.candidate with
      | _, None -> handle_end_of_candidates t
      | "edge", Some c when c##.candidate##indexOf eoc > 0 ->
         handle_end_of_candidates t
      | _, Some (c : _RTCIceCandidate Js.t) ->
         (* JSON.stringify doesn't work on some WebRTC objects anymore
				  See https://code.google.com/p/chromium/issues/detail?id=467366 *)
         if not t.webrtc.trickle then Lwt.return_ok () else (
           let (candidate : Api.Msg.candidate Js.t) = Js.Unsafe.obj [||] in
           candidate##.candidate := c##.candidate;
           candidate##.sdpMid := c##.sdpMid;
           candidate##.sdpMLineIndex := c##.sdpMLineIndex;
           send_trickle_candidate t candidate) in
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

  let init (t : t) (pc : _RTCPeerConnection Js.t) : unit =
    Log.ign_info_f
      "Preparing local SDP and gathering candidates (trickle=%b)"
      t.webrtc.trickle;
    pc##.oniceconnectionstatechange := Dom.handler (on_ice_conn_state_change t);
    pc##.onicecandidate := Dom.handler (on_ice_candidate t);
    pc##.ontrack := Dom.handler (on_track t)

  let create (t : t) : _RTCPeerConnection Js.t =
    let (pc_config : _RTCConfiguration Js.t) = Js.Unsafe.obj [||] in
    pc_config##.iceServers := Js.array @@ Array.of_list t.ice_servers;
    Option.iter (fun (x : ice_transport_policy) ->
        let v = Js.string @@ ice_transport_policy_to_string x in
        pc_config##.iceTransportPolicy := v) t.ice_transport_policy;
    Option.iter (fun (x : bundle_policy) ->
        let v = Js.string @@ bundle_policy_to_string x in
        pc_config##.bundlePolicy := v) t.bundle_policy;
    (* For Chrome versions before 72, we force a plan-b semantic,
     and unified-plan otherwise *)
    if check_browser ~browser:"chrome" ()
    then (
      let sdp_semantics =
        if check_browser ~ver:72 ~ver_cmp:(<) ()
        then "plan-b" else "unified-plan" in
      (Js.Unsafe.coerce pc_config)##.sdpSemantics := Js.string sdp_semantics);
    (* If this is Edge, enable BUNDLE explicitly *)
    if check_browser ~browser:"edge" ()
    then (
      let v = Js.string @@ bundle_policy_to_string Max_bundle in
      pc_config##.bundlePolicy := v);
    let pc_constraints = Js.Unsafe.(
        let dtls = obj [|"DtlsSrtpKeyAgreement", inject Js._true|] in
        let ipv6 = if not t.ipv6 then None
                   else obj [|"googIPv6", inject Js._true|] in
        let optional =
          List.map obj t.rtc_constraints
          |> List.cons dtls
          |> List.cons_maybe ipv6
          |> Array.of_list
          |> Js.array in
        obj [|"optional", inject optional|]) in
    Log.ign_info "Creating PeerConnection";
    Log.ign_debug ~inspect:pc_constraints "";
    let (pc_constr : (_RTCConfiguration Js.t ->
                      'a Js.t -> (* XXX constraints (legacy) *)
                      _RTCPeerConnection Js.t) Js.constr) =
      Js.Unsafe.global##.RTCPeerConnection in
    let pc = new%js pc_constr pc_config pc_constraints in
    Log.ign_debug ~inspect:pc "";
    pc

end

module Data_channel = struct

  let init (t : t) (dc : _RTCDataChannel Js.t) : unit =
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

  let create ~(media : Media.t)
        (pc : _RTCPeerConnection Js.t) : _RTCDataChannel Js.t =
    Log.ign_info "Creating data channel";
    let options = match media.data with
      | `Bool _ ->
         (* Create default options *)
         let (opts : _RTCDataChannelInit Js.t) = Js.Unsafe.obj [||] in
         opts##.ordered := Js._false;
         opts
      | `Options { ordered
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
    pc##createDataChannel (Js.string "JanusDataChannel") options

end

let munge_sdp_for_simulcasting (sdp : Js.js_string Js.t Js.optdef)
    : Js.js_string Js.t =
  (* FIXME implement *)
  match Js.Optdef.to_option sdp with
  | None -> Js.string ""
  | Some (sdp : Js.js_string Js.t) ->
     let lines = Js.str_array @@ sdp##split (Js.string "\r\n") in
     let cb = fun (s : Js.js_string Js.t) _ _ ->
       let regexp = new%js Js.regExp (Js.string "/m=(\w+) */") in
       let mline = Js.Opt.to_option @@ s##_match regexp in
       ignore mline;
       () in
     lines##forEach (Js.wrap_callback cb);
     sdp

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

(* TODO implement
let send_data
let send_dtmf
 *)

let find_transceivers (x : _RTCRtpTransceiver Js.t Js.js_array Js.t) =
  if x##.length <= 0 then None, None else
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
    x##reduce_init (Js.wrap_callback cb) (None, None)

let handle_transceiver (transceiver : _RTCRtpTransceiver Js.t option)
      (pc : _RTCPeerConnection Js.t)
      (track : Media.track) =
  let kind = match track.typ with
    | Audio _ -> "audio"
    | Video _ -> "video" in
  match Media.is_track_send_enabled track,
        Media.is_track_recv_enabled track,
        transceiver with
  | false, false, Some (tr : _RTCRtpTransceiver Js.t) ->
     (* Track disabled: have we removed it? *)
     if Media.should_remove_track track then (
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
     let tr = pc##addTransceiver (Js.string kind) init in
     Log.ign_info_f ~inspect:tr "Adding recvonly %s transceiver:" kind
  | _ -> ()

let handle_firefox_simulcast (pc : _RTCPeerConnection Js.t) : unit =
  (* Check if this is Firefox and we've been asked to do simulcasting *)
  if check_browser ~browser:"firefox" () then (
		(* FIXME Based on https://gist.github.com/voluntas/088bc3cc62094730647b *)
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
       sender##setParameters parameters)

let handle_chrome_simulcast (sdp : _RTCSessionDescriptionInit Js.t) =
  (* This SDP munging only works with chrome
     (Safari STP may support it too) *)
  if check_browser ~browser:"chrome" ()
     || check_browser ~browser:"safari" ()
  then (
    if String.equal "answer" (Js.to_string sdp##._type)
    then Log.ign_warning "simulcast=true, but this is an answer, and video \
                          breaks in Chrome if we enable it"
    else (
      Log.ign_info "Enabling Simulcasting for Chrome (SDP munging)";
      let new_sdp = munge_sdp_for_simulcasting sdp##.sdp in
      (Js.Unsafe.coerce sdp)##.sdp := new_sdp))
  else if not (check_browser ~browser:"firefox" ())
  then Log.ign_warning "simulcast=true, but this is not Chrome \
                        nor Firefox, ignoring"

let is_simulcast_needed ?(simulcast = false) (media : Media.t) : bool =
  simulcast && Media.is_track_send_enabled media.video

let create_offer_ ?(ice_restart = false) ?simulcast
      (media : Media.t) (t : t)
    : (_RTCSessionDescription Js.t option, string) Lwt_result.t =
  t.webrtc.pc
  |> Option.to_result_lazy (fun () ->
         "create_offer: RTCPeerConnection is not established")
  |> Lwt_result.lift
  >>= fun (pc : _RTCPeerConnection Js.t) ->
  Log.ign_info_f "Creating offer (iceDone=%b, simulcast=%s)"
    t.webrtc.ice_done (Option.to_string string_of_bool simulcast);
  let (media_constraints : _RTCOfferOptions Js.t) = Js.Unsafe.obj [||] in
  if check_browser ~browser:"firefox" ~ver:59 ~ver_cmp:(>=) ()
  then (
    (* Firefox >= 59 uses Transceivers *)
    let audio_transceiver, video_transceiver =
      find_transceivers pc##getTransceivers in
    (* Handle audio (and related changes, if any) *)
    handle_transceiver audio_transceiver pc media.audio;
    (* Handle video (and related changes, if any) *)
    handle_transceiver video_transceiver pc media.video)
  else (
    let audio_recv = Media.is_track_recv_enabled media.audio in
    let video_recv = Media.is_track_recv_enabled media.video in
    media_constraints##.offerToReceiveAudio := Js.bool audio_recv;
    media_constraints##.offerToReceiveVideo := Js.bool video_recv);
  if ice_restart then media_constraints##.iceRestart := Js._true;
  Log.ign_debug ~inspect:media_constraints "";
  let need_simulcast = is_simulcast_needed ?simulcast media in
  if need_simulcast then handle_firefox_simulcast pc;
  Lwt.try_bind (fun () ->
      Promise.to_lwt @@ pc##createOffer media_constraints)
    (fun (offer : _RTCSessionDescriptionInit Js.t) ->
      Log.ign_debug ~inspect:offer "";
      Log.ign_info "Setting local description";
      if need_simulcast then handle_chrome_simulcast offer;
      t.webrtc <- { t.webrtc with local_sdp = Some offer };
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
            let jsep = Js.Unsafe.(
                obj [| "type", inject offer##._type
                     ; "sdp", inject offer##.sdp |]) in
            Lwt.return_ok (Some jsep)))
        (fun e -> Lwt.return_error @@ exn_to_string e))
    (fun e -> Lwt.return_error @@ exn_to_string e)

let create_answer_ ?simulcast (media : Media.t) (t : t)
    : (_RTCSessionDescription Js.t option, string) Lwt_result.t =
  t.webrtc.pc
  |> Option.to_result_lazy (fun () ->
         "create_offer: RTCPeerConnection is not established")
  |> Lwt_result.lift
  >>= fun (pc : _RTCPeerConnection Js.t) ->
  Log.ign_info_f "Creating answer (iceDone=%b, simulcast=%s)"
    t.webrtc.ice_done (Option.to_string string_of_bool simulcast);
  let (media_constraints : _RTCAnswerOptions Js.t) = Js.Unsafe.obj [||] in
  if check_browser ~browser:"firefox" ~ver:59 ~ver_cmp:(>=) ()
     || check_browser ~browser:"chrome" ~ver:72 ~ver_cmp:(>=) ()
  then (
    (* Firefox >= 59 and Chrome >= 72 use Transceivers *)
    let audio_transceiver, video_transceiver =
      find_transceivers pc##getTransceivers in
    (* Handle audio (and related changes, if any) *)
    handle_transceiver audio_transceiver pc media.audio;
    (* Handle video (and related changes, if any) *)
    handle_transceiver video_transceiver pc media.video)
  else if check_browser ~browser:"firefox" ()
          || check_browser ~browser:"edge" ()
  then (
    let a = Media.is_track_recv_enabled media.audio in
    let v = Media.is_track_recv_enabled media.video in
    (Js.Unsafe.coerce media_constraints)##.offerToReceiveAudio := Js.bool a;
    (Js.Unsafe.coerce media_constraints)##.offerToReceiveVideo := Js.bool v)
  else (
    let a = Media.is_track_recv_enabled media.audio in
    let v = Media.is_track_recv_enabled media.video in
    let mandatory = Js.Unsafe.(
        obj [| "OfferToReceiveAudio", inject @@ Js.bool a
             ; "OfferToReceiveVideo", inject @@ Js.bool v |]) in
    (Js.Unsafe.coerce media_constraints)##.mandatory := mandatory);
  Log.ign_debug ~inspect:media_constraints "";
  let need_simulcast = is_simulcast_needed ?simulcast media in
  if need_simulcast then handle_firefox_simulcast pc;
  Lwt.try_bind (fun () ->
      Promise.to_lwt @@ pc##createAnswer media_constraints)
    (fun (answer : _RTCSessionDescriptionInit Js.t) ->
      Log.ign_debug ~inspect:answer "";
      Log.ign_info "Setting local description";
      if need_simulcast then handle_chrome_simulcast answer;
      t.webrtc <- { t.webrtc with local_sdp = Some answer };
      Lwt.try_bind (fun () -> Promise.to_lwt @@ pc##setLocalDescription answer)
        (fun () ->
          if not t.webrtc.ice_done && not t.webrtc.trickle
          then (
            Log.ign_info "Waiting for all candidates...";
            (* XXX original code do not call success callback here *)
            Lwt.return_ok None)
          else (
            (* JSON.stringify doesn't work on some WebRTC objects anymore
				       See https://code.google.com/p/chromium/issues/detail?id=467366 *)
            let jsep = Js.Unsafe.(
                obj [| "type", inject answer##._type
                     ; "sdp", inject answer##.sdp |]) in
            Lwt.return_ok (Some jsep)))
        (fun e -> Lwt.return_error @@ exn_to_string e))
    (fun e -> Lwt.return_error @@ exn_to_string e)

let update_stream
      (media : Media.track)
      (stream : mediaStream Js.t)
      (track : mediaStreamTrack Js.t)
      (pc : _RTCPeerConnection Js.t) =
  stream##addTrack track;
  let replace = match media.update with
    | Some Replace -> true | _ -> false in
  let kind = match media.typ with
    | Audio _ -> "audio"
    | Video _ -> "video" in
  if replace
     && check_browser ~browser:"firefox" ()
     && check_browser ~browser:"chrome" ~ver:72 ~ver_cmp:(>=) ()
  then (
    Log.ign_info_f ~inspect:track "Replacing %s track:" kind;
    let rec aux = function
      | [] -> ()
      | (sender : _RTCRtpSender Js.t) :: tl ->
         (match Js.Opt.to_option sender##.track with
          | None -> ()
          | Some (track' : mediaStreamTrack Js.t) ->
             if String.equal kind (Js.to_string track'##.kind)
             then ignore @@ sender##replaceTrack (Js.some track));
         aux tl in
    aux (Array.to_list @@ Js.to_array @@ pc##getSenders))
  else if check_browser ~browser:"firefox" ~ver:59 ~ver_cmp:(>=) ()
  then (
    let prefix = if replace then "Replacing" else "Adding" in
    Log.ign_info_f ~inspect:track "%s %s track:" prefix kind;
    let transceivers = pc##getTransceivers in
    let rec aux = function
      | [] -> None
      | (t : _RTCRtpTransceiver Js.t) :: tl ->
         match Js.Opt.to_option t##.sender##.track,
               Js.Opt.to_option t##.receiver##.track with
         | Some s_track, Some r_track ->
            if String.equal kind (Js.to_string s_track##.kind)
               && String.equal kind (Js.to_string r_track##.kind)
            then Some t else aux tl
         | _ -> aux tl in
    match aux @@ Array.to_list @@ Js.to_array transceivers with
    | None -> pc##addTrack track stream
    | Some (t : _RTCRtpTransceiver Js.t) ->
       ignore @@ t##.sender##replaceTrack (Js.some track))
  else (
    let prefix = if replace then "Replacing" else "Adding" in
    Log.ign_info_f ~inspect:track "%s %s track:" prefix kind;
    pc##addTrack track stream)

let handle_cached_candidates
      (candidates : _RTCIceCandidateInit Js.t Js.js_array Js.t)
      (pc : _RTCPeerConnection Js.t) : unit =
  if candidates##.length > 0 then (
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
    candidates##forEach (Js.wrap_callback cb);
    candidates##.length := 0)

type media_ext =
  { update : bool
  ; keep_audio : bool
  ; keep_video : bool
  ; media : Media.t
  }

let streams_done ?(jsep : _RTCSessionDescriptionInit Js.t option)
      ?(stream : mediaStream Js.t option)
      ({ update; media; _ } : media_ext)
      (t : t) =
  (* If we still need to create a PeerConnection, let's do that *)
  let (pc : _RTCPeerConnection Js.t) = match t.webrtc.pc with
    | Some pc -> pc
    | None ->
       let pc = Peer_connection.create t in
       t.webrtc <- { t.webrtc with pc = Some pc };
       Peer_connection.init t pc;
       pc in
  begin match t.webrtc.local_stream, update, t.webrtc.stream_external with
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
        let send_en = Media.is_track_send_enabled media.audio in
        match update, media.audio.update, send_en with
        | true, Some Add, _ | true, Some Replace, _ | false, _, true  ->
           update_stream media.audio my_stream track pc
        | _ -> ()
     end;
     let (video_track : mediaStreamTrack Js.t option) = match stream with
       | None -> None
       | Some s -> array_get s##getVideoTracks 0 in
     begin match video_track with
     | None -> ()
     | Some track ->
        let send_en = Media.is_track_send_enabled media.video in
        match update, media.video.update, send_en with
        | true, Some Add, _ | true, Some Replace, _ | false, _, true  ->
           update_stream media.video my_stream track pc
        | _ -> ()
     end;
  end;
  (* Any data channel to create? *)
  if Media.is_data_enabled ~media ()
  then (
    let dc = Data_channel.create ~media pc in
    t.webrtc <- { t.webrtc with data_channel = Some dc};
    Data_channel.init t dc);
  (* If there is a new local stream, let's notify the application *)
  begin match t.webrtc.local_stream with
  | None -> ()
  | Some (stream : mediaStream Js.t) ->
     Option.iter (fun f -> f stream) t.on_local_stream;
  end;
  (* Create offer/answer now *)
  begin match jsep with
  | None -> create_offer_ media t
  | Some (jsep : _RTCSessionDescriptionInit Js.t) ->
     Lwt.try_bind
       (fun () -> Promise.to_lwt @@ pc##setRemoteDescription jsep)
       (fun () ->
         Log.ign_info "Remote description accepted!";
         t.webrtc <- { t.webrtc with remote_sdp = Some jsep };
         (* Any trickle candidate we cached? *)
         handle_cached_candidates t.webrtc.candidates pc;
         create_answer_ media t)
       (fun e -> Lwt.return_error @@ exn_to_string e)
  end

let remove_track (typ : [`Audio | `Video]) (stream : mediaStream Js.t) =
  let kind, tracks = match typ with
    | `Audio -> "audio", stream##getAudioTracks
    | `Video -> "video", stream##getVideoTracks in
  match array_get tracks 0 with
  | None -> ()
  | Some (track : mediaStreamTrack Js.t) ->
     Log.ign_info_f ~inspect:track "Removing %s track" kind;
     stream##removeTrack track;
     try track##stop with _ -> ()

let replace_track (kind : string) (pc : _RTCPeerConnection Js.t) =
  let rec aux = function
    | [] -> ()
    | (sender : _RTCRtpSender Js.t) :: tl ->
       if check_browser ~browser:"firefox" ()
          || check_browser ~browser:"chrome" ~ver:72 ~ver_cmp:(>=) ()
       then () (* We can use replaceTrack *)
       else (
         (match Js.Opt.to_option sender##.track with
          | None -> ()
          | Some track ->
             if String.equal kind (Js.to_string track##.kind)
             then (
               Log.ign_info_f ~inspect:sender "Removing %s sender:" kind;
               pc##removeTrack sender));
         aux tl) in
  let senders = Array.to_list @@ Js.to_array pc##getSenders in
  aux senders

let remove_or_replace_tracks (media : Media.t)
      (local_stream : mediaStream Js.t option)
      (pc : _RTCPeerConnection Js.t option) : unit =
  begin match media.audio.update with
  | None | Some Add -> ()
  | Some Remove | Some Replace ->
     Option.iter (remove_track `Audio) local_stream;
     Option.iter (replace_track "audio") pc
  end;
  begin match media.video.update with
  | None | Some Add -> ()
  | Some Remove | Some Replace ->
     Option.iter (remove_track `Video) local_stream;
     Option.iter (replace_track "video") pc
  end

let update_track (local_stream : mediaStream Js.t option)
      (track : Media.track) : (bool * Media.track, string) result =
  match local_stream with
  | None ->
     (* No media stream: if we were asked to replace,
             it's actually and 'add' *)
     begin match Media.is_track_send_enabled track, track.update with
     | true, _ | _, Some Replace ->
        Ok (false, { track with update = Some Add })
     | _ -> Ok (false, track)
     end
  | Some s ->
     let kind, tracks = match track.typ with
       | Audio _ -> "audio", s##getAudioTracks
       | Video _ -> "video", s##getVideoTracks in
     match tracks##.length with
     | 0 ->
        (* No track: if we were asked to replace, it's actually an 'add' *)
        begin match Media.is_track_send_enabled track, track.update with
        | true, _ | _, Some Replace ->
           Ok (false, { track with update = Some Add })
        | _ -> Ok (false, track)
        end
     | _ ->
        (* We have a track: should we keep it as it is? *)
        begin match track.update with
        | Some Add ->
           let s =
             Printf.sprintf
               "Can't add %s stream, there is already one present"
               kind in
           Log.ign_error s;
           Error s
        | Some Remove | Some Replace -> Ok (false, track)
        | None -> Ok (Media.is_track_send_enabled track, track)
        end

let get_user_media ~simulcast (media : Media.t) =
  ignore simulcast;
  ignore media;
  Lwt.return_error ""

let check_peer_connection ?stream (media : Media.t) (t : t) =
  (* Are we updating a session? *)
  match t.webrtc.pc with
  | None ->
     (* Nope, no PeerConnection *)
     Ok { update = false
        ; keep_audio = false
        ; keep_video = false
        ; media }
  | Some (_ : _RTCPeerConnection Js.t) ->
     Log.ign_info "Updating existing media session";
     (* Check if there's anything to add/remove/replace, or if we
          can go directly to preparing the new SDP offer or answer *)
     match stream with
     | Some s ->
        (* External stream: is this the same as the one
             we were using before? *)
        if not (Option.equal ~eq:(==) (Some s) t.webrtc.local_stream)
        then Log.ign_info "Renegotiation involves a new external stream";
        Ok { update = true
           ; keep_audio = false
           ; keep_video = false
           ; media }
     | None ->
        (* FIXME add data channels *)
        Result.Infix.(
         let local_stream = t.webrtc.local_stream in
         (* Check if there are changes on audio*)
         update_track local_stream media.audio
         (* Check if there are changes on video *)
         >>= fun (keep_audio, audio) ->
         update_track local_stream media.video
         >|= fun (keep_video, video) ->
         let media = { media with audio; video } in
         { update = true
         ; keep_audio
         ; keep_video
         ; media })

let prepare_webrtc ?(simulcast = false) ?(trickle = true)
      ?(stream : mediaStream Js.t option)
      ?(jsep : _RTCSessionDescriptionInit Js.t option)
      (media : Media.t)
      (t : t) =
  t.webrtc <- { t.webrtc with trickle };
  check_peer_connection ?stream media t
  |> Lwt_result.lift
  >>= fun ({ media; update; keep_audio; keep_video } as media_ext) ->
  (* If we're updating and keeping all tracks,
     let's skip the getUserMedia part *)
  if update
     && Media.is_track_send_enabled media.audio && keep_audio
     && Media.is_track_send_enabled media.video && keep_video
  then streams_done ?jsep ?stream:t.webrtc.local_stream media_ext t
  else (
    (* If we're updating, check if we need to
       remove/replace one of the tracks *)
    if update && not t.webrtc.stream_external
    then remove_or_replace_tracks media t.webrtc.local_stream t.webrtc.pc;
    (* Was a MediaStream object passed, or do we need to take care of that? *)
    match stream with
    | Some (stream : mediaStream Js.t) ->
       Log.ign_info "MediaStream provided by the application";
       Log.ign_debug ~inspect:stream "";
       begin match update, t.webrtc.local_stream, t.webrtc.stream_external with
       | false, _, _ | _, None, _ | _, _, true -> ()
       | true, Some (local : mediaStream Js.t), false ->
          if local != stream then (
            (* We're replacing a stream we captured
             ourselves with an external one *)
            (try
               (* Try a MediaStreamTrack.stop() for each track *)
               let tracks = local##getTracks in
               let cb = fun (track : mediaStreamTrack Js.t) _ _ ->
                 Log.ign_debug ~inspect:track "";
                 track##stop in
               tracks##forEach (Js.wrap_callback cb)
             with _ -> ());
            t.webrtc <- { t.webrtc with local_stream = None })
       end;
       t.webrtc <- { t.webrtc with stream_external = true };
       streams_done ?jsep ~stream media_ext t
    | None ->
       if Media.is_track_send_enabled media.audio
          || Media.is_track_send_enabled media.video
       then get_user_media ~simulcast media
       else
         (* No need to do a getUserMedia, create offer/answer right away *)
         streams_done ?jsep ?stream media_ext t)

let prepare_webrtc_peer (jsep : _RTCSessionDescriptionInit Js.t)
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
         handle_cached_candidates t.webrtc.candidates pc;
         Lwt.return_ok ())
       (function
        | Js.Error e -> Lwt.return_error (Js.to_string e##toString)
        | Failure s -> Lwt.return_error s
        | e -> Lwt.return_error @@ Printexc.to_string e)

(* API *)

let id (t : t) : int =
  t.id

let typ (t : t) : typ =
  t.plugin

(* let volume (t : t) : unit =
 *   ignore t
 * 
 * let audio_muted (t : t) : bool =
 *   ignore t;
 *   false
 * 
 * let mute_audio (t : t) : unit =
 *   ignore t
 * 
 * let unmute_audio (t : t) : unit =
 *   ignore t
 * 
 * let video_muted (t : t) : bool =
 *   ignore t;
 *   false
 * 
 * let mute_video (t : t) : unit =
 *   ignore t
 * 
 * let unmute_video (t : t) : unit =
 *   ignore t
 * 
 * let bitrate (t : t) : unit =
 *   ignore t *)

let create_offer ?simulcast ?trickle ?stream
      ?audio ?video ?data (t : t) =
  let audio = match audio with
    | Some x -> x
    | None -> Media.make_audio (`Bool true) in
  let video = match video with
    | Some x -> x
    | None -> Media.make_video (`Bool true) in
  let data = match data with
    | Some x -> x
    | None -> `Bool false in
  let media = Media.{ audio; video; data } in
  prepare_webrtc ?simulcast ?trickle ?stream media t

let create_answer ?simulcast ?trickle ?jsep ?stream
      ?audio ?video ?data (t : t) =
  let audio = match audio with
    | Some x -> x
    | None -> Media.make_audio (`Bool true) in
  let video = match video with
    | Some x -> x
    | None -> Media.make_video (`Bool true) in
  let data = match data with
    | Some x -> x
    | None -> `Bool false in
  let media = Media.{ audio; video; data } in
  prepare_webrtc ?simulcast ?trickle ?stream ?jsep media t

let handle_remote_jsep (jsep : _RTCSessionDescriptionInit Js.t)
      (t : t) : (unit, string) Lwt_result.t =
  prepare_webrtc_peer jsep t

(* let send_data (t : t) (text : string) : unit =
 *   Log.ign_info_f "Sending string on data channel: %s" text;
 *   ignore t
 * 
 * let dtmf (t : t) : unit =
 *   ignore t *)

let hangup ?(request = true) (t : t) : unit =
  Log.ign_info "Cleaning WebRTC stuff";
  if request
  then (
    (* Send a hangup request (we don't really care about the response) *)
    let request =
      Api.Msg.make_req
        ?token:t.token
        ?apisecret:t.apisecret
        ~janus:"hangup"
        ~transaction:(String.random 12)
        () in
    Log.ign_debug_f "Sending hangup request (handle=%d):" t.id;
    Log.ign_debug ~inspect:request "";
    (* TODO add websockets *)
    Api.http_call ~meth:`POST
      ~body:request
      (Printf.sprintf "%s/%d/%d" t.server t.session_id t.id)
    |> Lwt.ignore_result);
  (* Cleanup stack *)
  (* TODO clear timers *)
  (* Try a MediaStreamTrack.stop() for each track *)
  begin match t.webrtc.stream_external, t.webrtc.local_stream with
  | false, Some (stream : mediaStream Js.t) ->
     (try
        Log.ign_info "Stopping local stream tracks";
        let tracks = stream##getTracks in
        let cb = fun (track : mediaStreamTrack Js.t) _ _ ->
          Log.ign_info ~inspect:track "";
          track##stop in
        tracks##forEach (Js.wrap_callback cb)
      with _ -> ())
  | _ -> ()
  end;
  (* Close PeerConnection *)
  begin match t.webrtc.pc with
  | None -> ()
  | Some (pc : _RTCPeerConnection Js.t) ->
     try pc##close with _ -> ()
  end;
  let upd =
    { t.webrtc with remote_stream = None
                  ; stream_external = false
                  ; local_stream = None
                  ; pc = None
                  ; candidates = new%js Js.array_empty
                  ; local_sdp = None
                  ; remote_sdp = None
                  ; ice_done = false
                  ; data_channel = None
                  ; dtmf_sender = None
    } in
  t.webrtc <- upd;
  Option.iter (fun f -> f ()) t.on_cleanup

let detach ?(async = true) (t : t) : (unit, string) Lwt_result.t =
  Log.ign_info_f "Destroying handle %d (async=%b)" t.id async;
  hangup t;
  if t.detached
  then (
		(* Plugin was already detached by Janus, calling detach again will
       return a handle not found error, so just exit here *)
    t.rm_from_session t.id;
    Lwt.return_ok ())
  else (
    is_connected_lwt t.is_connected
    >>= fun () ->
    let request =
      Api.Msg.make_req
        ?token:t.token
        ?apisecret:t.apisecret
        ~janus:"detach"
        ~transaction:(String.random 12)
        () in
    (* TODO add websockets *)
    Lwt.Infix.(
      Api.http_call ~meth:`POST
        ~async
        ~body:request
        (Printf.sprintf "%s/%d/%d" t.server t.session_id t.id)
      >>= function
      | Error e ->
         t.rm_from_session t.id;
         Lwt.return_error @@ Api.error_to_string e
      | Ok msg ->
         match Api.Msg.check_msg msg with
         | Error e ->
            t.rm_from_session t.id;
            Lwt.return_error e
         | Ok msg ->
            Log.ign_info "Destroyed handle:";
            Log.ign_debug ~inspect:msg "";
            t.rm_from_session t.id;
            Lwt.return_ok ()))
