open Js_of_ocaml
open Utils
open Webrtc
open Adapter
open Lwt_result.Infix

include Plugin_types

let munge_sdp_for_simulcasting (sdp : Js.js_string Js.t Js.optdef)
    : Js.js_string Js.t =
  (* FIXME implement *)
  Js.Optdef.get sdp (fun () -> Js.string "")

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
     let tr = pc##addTransceiver (Js.string kind) init in
     Log.ign_info_f ~inspect:tr "Adding recvonly %s transceiver:" kind
  | _ -> ()

let handle_simulcasting ~video_send ~simulcast pc : unit =
  (* Check if this is Firefox and we've been asked to do simulcasting *)
  if video_send && simulcast && check_browser ~browser:"firefox" () then (
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
  let audio_send = Media.is_audio_send_enabled ~media () in
  let audio_recv = Media.is_audio_recv_enabled ~media () in
  let video_send = Media.is_video_send_enabled ~media () in
  let video_recv = Media.is_video_recv_enabled ~media () in
  if check_browser ~browser:"firefox" ~ver:59 ~ver_cmp:(>=) ()
  then (
    (* Firefox >= 59 uses Transceivers *)
    let audio_transceiver, video_transceiver =
      find_transceivers pc##getTransceivers in
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
    media_constraints##.offerToReceiveVideo := Js.bool video_recv);
  if ice_restart then media_constraints##.iceRestart := Js._true;
  Log.ign_debug ~inspect:media_constraints "";
  handle_simulcasting ~video_send ~simulcast pc;
  Lwt.try_bind (fun () ->
      Promise.to_lwt @@ pc##createOffer media_constraints)
    (fun (offer : _RTCSessionDescriptionInit Js.t) ->
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

let create_answer ?(simulcast = false)
      (media : Media.t) (t : t)
    : (_RTCSessionDescription Js.t option, string) Lwt_result.t =
  t.webrtc.pc
  |> Option.to_result_lazy (fun () ->
         "create_offer: RTCPeerConnection is not established")
  |> Lwt_result.lift
  >>= fun (pc : _RTCPeerConnection Js.t) ->
  Log.ign_info_f "Creating answer (iceDone=%b, simulcast=%b)"
    t.webrtc.ice_done simulcast;
  let (media_constraints : _RTCAnswerOptions Js.t) = Js.Unsafe.obj [||] in
  let audio_send = Media.is_audio_send_enabled ~media () in
  let audio_recv = Media.is_audio_recv_enabled ~media () in
  let video_send = Media.is_video_send_enabled ~media () in
  let video_recv = Media.is_video_recv_enabled ~media () in
  if check_browser ~browser:"firefox" ~ver:59 ~ver_cmp:(>=) ()
     || check_browser ~browser:"chrome" ~ver:72 ~ver_cmp:(>=) ()
  then (
    (* Firefox >= 59 and Chrome >= 72 use Transceivers *)
    let audio_transceiver, video_transceiver =
      find_transceivers pc##getTransceivers in
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
      video_transceiver pc `Video;
    ()
  )
  else if check_browser ~browser:"firefox" ()
          || check_browser ~browser:"edge" ()
  then (
    let a = Js.bool audio_recv in
    let v = Js.bool video_recv in
    (Js.Unsafe.coerce media_constraints)##.offerToReceiveAudio := a;
    (Js.Unsafe.coerce media_constraints)##.offerToReceiveVideo := v;
  )
  else (
    let mandatory = Js.Unsafe.(
        obj [| "OfferToReceiveAudio", inject @@ Js.bool audio_recv
             ; "OfferToReceiveVideo", inject @@ Js.bool video_recv |]) in
    (Js.Unsafe.coerce media_constraints)##.mandatory := mandatory;
  );
  Log.ign_debug ~inspect:media_constraints "";
  handle_simulcasting ~video_send ~simulcast pc;
  Lwt.try_bind (fun () ->
      Promise.to_lwt @@ pc##createAnswer media_constraints)
    (fun (answer : _RTCSessionDescriptionInit Js.t) ->
      Log.ign_debug ~inspect:answer "";
      Log.ign_info "Setting local description";
      if video_send && simulcast then (
        if check_browser ~browser:"chrome" () then (
          (* FIXME Apparently trying to simulcast when answering
             breaks video in Chrome... *)
          (* Log.ign_info "Enabling Simulcasting for Chrome (SDP munging)";
           * let new_sdp = munge_sdp_for_simulcasting answer##.sdp in
           * (Js.Unsafe.coerce answer)##.sdp := new_sdp *)
          Log.ign_warning "simulcast=true, but this is an answer, and video \
                           breaks in Chrome if we enable it")
        else if not (check_browser ~browser:"firefox" ())
        then Log.ign_warning "simulcast=true, but this is not Chrome \
                              nor Firefox, ignoring");
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

let streams_done ?(jsep : _RTCSessionDescriptionInit Js.t option)
      ?(stream : mediaStream Js.t option)
      (media : Media.t)
      (t : t) =
  (* If we still need to create a PeerConnection, let's do that *)
  let (pc : _RTCPeerConnection Js.t) = match t.webrtc.pc with
    | Some pc -> pc
    | None ->
       let pc = Peer_connection.create t in
       t.webrtc <- { t.webrtc with pc = Some pc };
       Peer_connection.init t pc;
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
        if ((not update && Media.is_audio_send_enabled ~media ())
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
        if ((not update && Media.is_video_send_enabled ~media ())
            || (update && (add_video || replace_video)))
        then update_video_stream ~replace_video my_stream track pc
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
  | None -> create_offer media t
  | Some (jsep : _RTCSessionDescriptionInit Js.t) ->
     Lwt.try_bind
       (fun () -> Promise.to_lwt @@ pc##setRemoteDescription jsep)
       (fun () ->
         Log.ign_info "Remote description accepted!";
         t.webrtc <- { t.webrtc with remote_sdp = Some jsep };
         (* Any trickle candidate we cached? *)
         handle_cached_candidates t.webrtc.candidates pc;
         create_answer media t)
       (fun e -> Lwt.return_error @@ exn_to_string e)
  end

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

let handle_remote_jsep (jsep : _RTCSessionDescriptionInit Js.t)
      (t : t) : (unit, string) Lwt_result.t =
  prepare_webrtc_peer jsep t

let send_data (t : t) (text : string) : unit =
  Log.ign_info_f "Sending string on data channel: %s" text;
  ignore t

let dtmf (t : t) : unit =
  ignore t

let hangup (t : t) : unit =
  ignore t

let detach (t : t) : unit =
  ignore t

