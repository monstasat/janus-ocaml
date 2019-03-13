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
        | _ -> atr' in
      let vtr = match atr, vtr', strack, rtrack with
        | None, None, Some s, Some r ->
           if String.equal "video" (Js.to_string s##.kind)
              && String.equal "video" (Js.to_string r##.kind)
           then Some tr else None
        | _ -> vtr' in
      atr, vtr in
    x##reduce_init (Js.wrap_callback cb) (None, None)

let handle_transceiver ~(kind : string)
      ~(recv : bool)
      ~(send : bool)
      ~(remove : bool)
      (transceiver : _RTCRtpTransceiver Js.t option)
      (pc : _RTCPeerConnection Js.t) =
  match send, recv, transceiver with
  | false, false, Some (tr : _RTCRtpTransceiver Js.t) ->
     (* Track disabled: have we removed it? *)
     if remove then (
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

let is_simulcast_needed ?(simulcast = false) (source : Media.source) : bool =
  let video_send = match source with
    | `Stream _ -> true
    | `Media { video; _ } -> Media.is_track_send_enabled video in
  simulcast && video_send

let need_remove_video : Media.source -> bool = function
  | `Stream _ -> false
  | `Media { video; _ } ->
     match video.update with
     | Some Remove -> true
     | _ -> false

let need_remove_audio : Media.source -> bool = function
  | `Stream _ -> false
  | `Media { audio; _ } ->
     match audio.update with
     | Some Remove -> true
     | _ -> false

let need_send_video : Media.source -> bool = function
  | `Stream _ -> true
  | `Media { video; _ } ->
     match video.source with
     | `Bool x -> x
     | _ -> true

let need_send_audio : Media.source -> bool = function
  | `Stream _ -> true
  | `Media { audio; _ } ->
     match audio.source with
     | `Bool x -> x
     | _ -> true

let create_offer_ ?(ice_restart = false) ?simulcast
      ~(audio_recv : bool)
      ~(video_recv : bool)
      (sdp_thread : _RTCSessionDescriptionInit Js.t Lwt.t)
      (source : Media.source)
      (t : t) : (_RTCSessionDescriptionInit Js.t, string) Lwt_result.t =
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
    handle_transceiver ~kind:"audio"
      ~recv:audio_recv
      ~send:(need_send_audio source)
      ~remove:(need_remove_audio source)
      audio_transceiver pc;
    (* Handle video (and related changes, if any) *)
    handle_transceiver ~kind:"video"
      ~recv:video_recv
      ~send:(need_send_video source)
      ~remove:(need_remove_video source)
      video_transceiver pc)
  else (
    media_constraints##.offerToReceiveAudio := Js.bool audio_recv;
    media_constraints##.offerToReceiveVideo := Js.bool video_recv);
  if ice_restart then media_constraints##.iceRestart := Js._true;
  Log.ign_debug ~inspect:media_constraints "";
  let need_simulcast = is_simulcast_needed ?simulcast source in
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
            Lwt.map (fun x -> Ok x) sdp_thread)
          else (
            Log.ign_info "Offer ready";
            (* JSON.stringify doesn't work on some WebRTC objects anymore
				       See https://code.google.com/p/chromium/issues/detail?id=467366 *)
            let (jsep : _RTCSessionDescriptionInit Js.t) = Js.Unsafe.(
                obj [| "type", inject offer##._type
                     ; "sdp", inject offer##.sdp |]) in
            Lwt.cancel sdp_thread;
            Lwt.return_ok jsep))
        (fun e -> Lwt.return_error @@ exn_to_string e))
    (fun e -> Lwt.return_error @@ exn_to_string e)

let create_answer_ ?simulcast
      ~(audio_recv : bool)
      ~(video_recv : bool)
      (sdp_thread : _RTCSessionDescriptionInit Js.t Lwt.t)
      (source : Media.source)
      (t : t) : (_RTCSessionDescriptionInit Js.t, string) Lwt_result.t =
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
    handle_transceiver ~kind:"audio"
      ~recv:audio_recv
      ~send:(need_send_audio source)
      ~remove:(need_remove_audio source)
      audio_transceiver pc;
    (* Handle video (and related changes, if any) *)
    handle_transceiver ~kind:"video"
      ~recv:video_recv
      ~send:(need_send_video source)
      ~remove:(need_remove_video source)
      video_transceiver pc)
  else if check_browser ~browser:"firefox" ()
          || check_browser ~browser:"edge" ()
  then (
    let c = Js.Unsafe.coerce media_constraints in
    c##.offerToReceiveAudio := Js.bool audio_recv;
    c##.offerToReceiveVideo := Js.bool video_recv)
  else (
    let mandatory = Js.Unsafe.(
        obj [| "OfferToReceiveAudio", inject @@ Js.bool audio_recv
             ; "OfferToReceiveVideo", inject @@ Js.bool video_recv |]) in
    (Js.Unsafe.coerce media_constraints)##.mandatory := mandatory);
  Log.ign_debug ~inspect:media_constraints "";
  let need_simulcast = is_simulcast_needed ?simulcast source in
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
            Lwt.map (fun x -> Ok x) sdp_thread)
          else (
            (* JSON.stringify doesn't work on some WebRTC objects anymore
				       See https://code.google.com/p/chromium/issues/detail?id=467366 *)
            let (jsep : _RTCSessionDescriptionInit Js.t) = Js.Unsafe.(
                obj [| "type", inject answer##._type
                     ; "sdp", inject answer##.sdp |]) in
            Lwt.cancel sdp_thread;
            Lwt.return_ok jsep))
        (fun e -> Lwt.return_error @@ exn_to_string e))
    (fun e -> Lwt.return_error @@ exn_to_string e)

let update_stream
      ~(typ:[`Audio | `Video])
      (media : 'a Media.track)
      (stream : mediaStream Js.t)
      (track : mediaStreamTrack Js.t)
      (pc : _RTCPeerConnection Js.t) =
  stream##addTrack track;
  let replace = match media.update with
    | Some Replace -> true | _ -> false in
  let kind = match typ with
    | `Audio -> "audio"
    | `Video -> "video" in
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
    | None -> ignore @@ pc##addTrack track stream
    | Some (t : _RTCRtpTransceiver Js.t) ->
       ignore @@ t##.sender##replaceTrack (Js.some track))
  else (
    let prefix = if replace then "Replacing" else "Adding" in
    Log.ign_info_f ~inspect:track "%s %s track:" prefix kind;
    ignore @@ pc##addTrack track stream)

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

let add_tracks (pc : _RTCPeerConnection Js.t) (stream : mediaStream Js.t) =
  Log.ign_info "Adding local stream";
  let tracks = stream##getTracks in
  let cb = fun (track : mediaStreamTrack Js.t) _ _ ->
    Log.ign_info ~inspect:track "Adding local track:";
    ignore @@ pc##addTrack track stream in
  tracks##forEach (Js.wrap_callback cb)

let streams_done ?(jsep : _RTCSessionDescriptionInit Js.t option)
      ?(data : Media.data option)
      ?(stream : mediaStream Js.t option)
      ~(audio_recv : bool)
      ~(video_recv : bool)
      (source : Media.source)
      (t : t) =
  let sdp_thread, w = Lwt.task () in
  Log.ign_debug ~inspect:(Js.Opt.option stream) "streams done:";
  Option.iter (fun (s : mediaStream Js.t) ->
      Log.ign_debug ~inspect:s##getAudioTracks " -- Audio tracks:";
      Log.ign_debug ~inspect:s##getVideoTracks " -- Video tracks:")
    stream;
  (* We're now capturing the new stream: check if we're updating or it's a new thing *)
  let (pc : _RTCPeerConnection Js.t) =
    match t.webrtc.local_stream, t.webrtc.pc, source with
    | Some my_stream, Some pc, `Media media ->
       (* We only need to update the existing stream *)
       let (audio_track : mediaStreamTrack Js.t option) =
         match stream with
         | None -> None
         | Some s -> array_get s##getAudioTracks 0 in
       begin match audio_track with
       | None -> ()
       | Some track ->
          match media.audio.update with
          | Some Add | Some Replace ->
             update_stream ~typ:`Audio media.audio my_stream track pc
          | _ -> ()
       end;
       let (video_track : mediaStreamTrack Js.t option) =
         match stream with
         | None -> None
         | Some s -> array_get s##getVideoTracks 0 in
       begin match video_track with
       | None -> ()
       | Some track ->
          match media.video.update with
          | Some Add | Some Replace ->
             update_stream ~typ:`Video media.video my_stream track pc
          | _ -> ()
       end;
       pc
    | _ ->
       t.webrtc <- { t.webrtc with local_stream = stream };
       (* If we still need to create a PeerConnection, let's do that *)
       let (pc : _RTCPeerConnection Js.t) = match t.webrtc.pc with
         | Some pc -> pc
         | None ->
            let pc = Peer_connection.create t in
            t.webrtc <- { t.webrtc with pc = Some pc };
            Peer_connection.init w pc t;
            pc in
       Option.iter (add_tracks pc) t.webrtc.local_stream;
       pc in
  (* Any data channel to create? *)
  (match t.webrtc.data_channel, data with
   | Some _, _ | None, None | None, Some `Bool false -> ()
   | None, Some `Bool true ->
      let dc = Data_channel.(create (make_default_init ()) pc) in
      t.webrtc <- { t.webrtc with data_channel = Some dc };
      Data_channel.init t dc
   | None, Some `Init init ->
      let dc = Data_channel.create init pc in
      t.webrtc <- { t.webrtc with data_channel = Some dc };
      Data_channel.init t dc);
  (* If there is a new local stream, let's notify the application *)
  (match t.webrtc.local_stream, t.on_local_stream with
   | Some (s : mediaStream Js.t), Some f -> f s t
   | _ -> ());
  (* Create offer/answer now *)
  match jsep with
  | None -> create_offer_ ~audio_recv ~video_recv sdp_thread source t
  | Some (jsep : _RTCSessionDescriptionInit Js.t) ->
     Lwt.try_bind
       (fun () -> Promise.to_lwt @@ pc##setRemoteDescription jsep)
       (fun () ->
         Log.ign_info "Remote description accepted!";
         t.webrtc <- { t.webrtc with remote_sdp = Some jsep };
         (* Any trickle candidate we cached? *)
         handle_cached_candidates t.webrtc.candidates pc;
         create_answer_ ~audio_recv ~video_recv sdp_thread source t)
       (Lwt.return_error % exn_to_string)

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
  aux @@ Array.to_list @@ Js.to_array pc##getSenders

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

let update_track ~(typ:[`Audio | `Video])
      (local_stream : mediaStream Js.t option)
      (track : 'a Media.track)
    : (bool * 'a Media.track, string) result =
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
     let kind, tracks = match typ with
       | `Audio -> "audio", s##getAudioTracks
       | `Video -> "video", s##getVideoTracks in
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

let prepare_webrtc ?(simulcast = false) ?(trickle = true)
      ?(data : Media.data option)
      ?(jsep : _RTCSessionDescriptionInit Js.t option)
      ~(audio_recv : bool)
      ~(video_recv : bool)
      (source : Media.source)
      (t : t) =
  t.webrtc <- { t.webrtc with trickle };
  match source with
  | `Stream (stream : mediaStream Js.t) ->
     Log.ign_info "MediaStream provided by the application";
     Log.ign_debug ~inspect:stream "";
     if Option.is_some t.webrtc.pc
        && not (Option.equal ~eq:(==) (Some stream) t.webrtc.local_stream)
     then Log.ign_info "Renegotiation involves a new external stream";
     (* If we're updating, let's check if we need to release the previous stream *)
     begin match t.webrtc.pc,
                 t.webrtc.local_stream,
                 t.webrtc.stream_external with
     | Some _, Some (local : mediaStream Js.t), false ->
        (* We're updating, we have a previous stream and it is not external *)
        if local != stream then (
          (* We're replacing a stream we captured ourselves with an external one *)
          (try
             (* Try a MediaStreamTrack.stop() for each track *)
             let tracks = local##getTracks in
             let cb = fun (track : mediaStreamTrack Js.t) _ _ ->
               Log.ign_debug ~inspect:track "";
               track##stop in
             tracks##forEach (Js.wrap_callback cb)
           with _ -> ());
          t.webrtc <- { t.webrtc with local_stream = None })
     | _ -> ()
     end;
     t.webrtc <- { t.webrtc with stream_external = true };
     streams_done ?jsep ?data ~stream ~audio_recv ~video_recv source t
  | `Media ({ audio; video; _ } as media : Media.t) ->
     Result.Infix.(
      let local_stream = t.webrtc.local_stream in
      (* Check if there are changes on audio*)
      update_track ~typ:`Audio local_stream media.audio
      (* Check if there are changes on video *)
      >>= fun (keep_audio, audio) ->
      update_track ~typ:`Video local_stream media.video
      >|= fun (keep_video, video) ->
      let media = Media.{ audio; video } in
      keep_video, keep_audio, media)
     |> Lwt_result.lift
     >>= fun (keep_audio, keep_video, media) ->
     (* If we're keeping all tracks, let's skip the getUserMedia part *)
     if keep_audio && keep_video
     then streams_done ?jsep ?data ?stream:t.webrtc.local_stream
            ~audio_recv ~video_recv source t
     else (
       (* Check if we need to remove/replace one of the tracks *)
       remove_or_replace_tracks media t.webrtc.local_stream t.webrtc.pc;
       if Media.(is_track_send_enabled audio || is_track_send_enabled video)
       then
         User_media.get_user_media ?jsep
           ~keep_audio ~keep_video
           ~simulcast media t
         >>= fun stream ->
         streams_done ?jsep ?data ~stream ~audio_recv ~video_recv source t
       else
         (* No need to do a getUserMedia, create offer/answer right away *)
         streams_done ?jsep ?data ~audio_recv ~video_recv source t)

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

let create_offer ?simulcast ?trickle
      ?(data : Media.data option)
      ?(audio_recv = true)
      ?(video_recv = true)
      (source : Media.source)
      (t : t) =
  prepare_webrtc ?simulcast ?trickle ?data
    ~audio_recv ~video_recv source t

let create_answer ?simulcast ?trickle
      ?(data : Media.data option)
      ?(audio_recv = true)
      ?(video_recv = true)
      ~(jsep : _RTCSessionDescriptionInit Js.t)
      (source : Media.source)
      (t : t) =
  prepare_webrtc ?simulcast ?trickle ?data ~jsep
    ~audio_recv ~video_recv source t

let handle_remote_jsep (jsep : _RTCSessionDescriptionInit Js.t)
      (t : t) : (unit, string) Lwt_result.t =
  prepare_webrtc_peer jsep t

module Data_channel = struct

  let check_dc (f : _RTCDataChannel Js.t -> unit) (t : t) =
    match t.webrtc.data_channel with
    | None -> Lwt.return_error "Can't send data. RTCDataChannel is not available"
    | Some (x : _RTCDataChannel Js.t) ->
       try Lwt.return_ok @@ f x
       with exn -> Lwt.return_error @@ exn_to_string exn

  let get_ready_state (t : t) : string option =
    match t.webrtc.data_channel with
    | None -> None
    | Some (x : _RTCDataChannel Js.t) ->
       Some (Js.to_string x##.readyState)

  let send_string (text : string)
        (t : t) : (unit, string) Lwt_result.t =
    check_dc (fun x ->
        Log.ign_warning ~inspect:x "DC";
        x##send_string (Js.string text)) t

  let send_js_string (text : Js.js_string Js.t)
        (t : t) : (unit, string) Lwt_result.t =
    check_dc (fun x -> x##send_string text) t

  let send_blob (blob : #File.blob Js.t)
        (t : t) : (unit, string) Lwt_result.t =
    check_dc (fun x -> x##send_blob blob) t

  let send_array_buffer (ab : #Typed_array.arrayBuffer Js.t)
        (t : t) : (unit, string) Lwt_result.t =
    check_dc (fun x -> x##send_arrayBuffer ab) t

  let send_array_buffer_view (abv : #Typed_array.arrayBufferView Js.t)
        (t : t) : (unit, string) Lwt_result.t =
    check_dc (fun x -> x##send_arrayBufferView abv) t

end

module Stats = struct

  type bitrate =
    { audio : int option
    ; video : int option
    }

  let find_rtp_stats (kind : string)
        (stats : _RTCStats Js.t)
      : (_RTCInboundRtpStreamStats Js.t option) =
    let (media_type : string option) =
      Option.map Js.to_string
      @@ Js.Optdef.to_option
      @@ (Js.Unsafe.coerce stats)##.mediaType in
    let (kind' : string option) =
      Option.map Js.to_string
      @@ Js.Optdef.to_option
      @@ (Js.Unsafe.coerce stats)##.kind in
    if (Option.equal ~eq:String.equal media_type (Some kind)
        || Option.equal ~eq:String.equal kind' (Some kind)
        || stats##.id##toLowerCase##indexOf (Js.string kind) > (-1))
       && String.equal (Js.to_string stats##._type) "inbound-rtp"
       && stats##.id##indexOf (Js.string "rtcp") < 0
    then Some (Js.Unsafe.coerce stats)
    else None

  let get_track_bitrate (stats : _RTCInboundRtpStreamStats Js.t)
        (br : track_bitrate) =
    let ts = stats##.timestamp in
    let bs = stats##.bytesReceived in
    match br.bytes, br.timestamp with
    | Some prev_bs, Some prev_ts ->
       (* in seconds *)
       let time_passed =
         (* Apparently the timestamp is in microseconds in Safari *)
         if Adapter.check_browser ~browser:"safari" ()
         then (ts -. prev_ts) /. 1_000_000.
         else (ts -. prev_ts) /. 1_000. in
       (* Bytes per second *)
       let (bitrate : int) =
         (float_of_int (bs - prev_bs)) *. 8. /. time_passed
         |> (fun x -> Float.floor (x +. 0.5))
         |> int_of_float in
       { value = Some bitrate; bytes = Some bs; timestamp = Some ts }
    | _ -> { br with bytes = Some bs; timestamp = Some ts }

  let start_bitrate_loop ?(period = 1000.)
        (f : bitrate -> unit)
        (t : t) =
    Option.to_result_lazy (fun () -> "Invalid PeerConnection") t.webrtc.pc
    |> Lwt.return
    >>= fun (pc : _RTCPeerConnection Js.t) ->
    if not @@ Js.Optdef.test (Js.Unsafe.coerce pc)##.getStats
    then (
      let s = "Getting the video bitrate is unsupported by this browser" in
      Log.ign_warning s;
      Lwt.return_error s)
    else
      match t.bitrate.timer with
      | Some _ -> Lwt.return_ok ()
      | None ->
         Log.ign_info "Starting bitrate timer (via getStats)";
         let cb = fun () ->
           (Lwt.try_bind (fun () -> Promise.to_lwt @@ pc##getStats Js.null)
              (fun (stats : _RTCStatsReport Js.t) -> Lwt.return_ok stats)
              (Lwt.return_error % exn_to_string)
            >|= fun (stats : _RTCStatsReport Js.t) ->
            let rec aux (iter : _RTCStats Js.t iterator Js.t) ((a, v) as acc) =
              let next = iter##next in
              match Js.to_bool next##._done, Js.Optdef.to_option next##.value with
              | true, _ -> acc
              | false, None -> aux iter acc
              | false, Some value ->
                 match a, v with
                 | (Some _, Some _) as res -> res
                 | None, (Some _ as v) -> aux iter (find_rtp_stats "audio" value, v)
                 | (Some _ as a), None -> aux iter (a, find_rtp_stats "video" value)
                 | None, None ->
                    aux iter (find_rtp_stats "audio" value,
                              find_rtp_stats "video" value) in
            let a, v = aux stats##values (None, None) in
            let v' = match v with
              | None ->
                 let br = { value = None; bytes = None; timestamp = None } in
                 t.bitrate <- { t.bitrate with video = br };
                 br.value
              | Some x ->
                 let br = get_track_bitrate x t.bitrate.video in
                 t.bitrate <- { t.bitrate with video = br };
                 br.value in
            let a' = match a with
              | None ->
                 let br = { value = None; bytes = None; timestamp = None } in
                 t.bitrate <- { t.bitrate with audio = br };
                 br.value
              | Some x ->
                 let br = get_track_bitrate x t.bitrate.audio in
                 t.bitrate <- { t.bitrate with audio = br };
                 br.value in
            f { audio = a'; video = v' })
           |> Lwt.ignore_result in
         let timer = Dom_html.window##setInterval (Js.wrap_callback cb) period in
         t.bitrate <- { t.bitrate with timer = Some timer };
         Lwt.return_ok ()

  let stop_bitrate_loop (t : t) =
    match t.bitrate.timer with
    | None -> ()
    | Some tmr ->
       Dom_html.window##clearInterval tmr;
       t.bitrate <- { t.bitrate with timer = None }

end

let send_message ?(message : 'a Js.t option)
      ?(jsep : _RTCSessionDescriptionInit Js.t option)
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
  Option.iter (fun x -> Dom_html.window##clearInterval x) t.bitrate.timer;
  t.bitrate <- { t.bitrate with timer = None };
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
  Option.iter (fun f -> f t) t.on_cleanup

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
