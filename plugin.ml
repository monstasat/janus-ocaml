open Js_of_ocaml
open Utils
open Types
open Media_stream
open Adapter

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

module SDP = struct

  type t =
    { type_ : 'a. 'a Js.t
    ; sdp : 'a. 'a Js.t
    ; trickle : bool option
    }

end

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
    ; local_sdp : SDP.t option
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
  ; plugin : string
  ; token : string option
  ; ice_servers : _RTCIceServer Js.t list
  ; ice_transport_policy : ice_transport_policy option
  ; bundle_policy : bundle_policy option
  ; logs : logs
  ; callbacks : callbacks
  ; mutable webrtc_stuff : Webrtc_stuff.t
  ; mutable detached : bool
  }

let is_data_enabled ?(media : Media.t option) (logs : logs) : bool =
  match get_browser () with
  | "edge" ->
     logs.warn.str "Edge doen't support data channels yet";
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

(* let prepare_webrtc ?jsep
 *       ?(stream : mediaStream Js.t option)
 *       (media : Media.t)
 *       (t : t) =
 *   let config = t.webrtc_stuff in
 *   (\* Are we updating a session ? *\)
 *   match config.pc with
 *   | None -> ()
 *   | Some _ ->
 *      t.logs.info.str "Updating existing media session";
 *      () *)

let send_sdp (t : t) = ignore t

let send_trickle_candidate (t : t) (_ : 'a Js.t) = ignore t

let update_audio_stream ~(replace_audio : bool)
      (stream : mediaStream Js.t)
      (track : mediaStreamTrack Js.t) =
  stream##addTrack track;
  if replace_audio
     && check_browser ~browser:"firefox" ()
     && check_browser ~browser:"chrome" ~ver:72 ~ver_cmp:(>=) ()
  then ()
  else if check_browser ~browser:"firefox" ~ver:59 ~ver_cmp:(>=) ()
  then ()
  else ()

let update_video_stream ~(replace_video : bool)
      (stream : mediaStream Js.t)
      (track : mediaStreamTrack Js.t) =
  stream##addTrack track;
  if replace_video
     && check_browser ~browser:"firefox" ()
     && check_browser ~browser:"chrome" ~ver:72 ~ver_cmp:(>=) ()
  then ()
  else if check_browser ~browser:"firefox" ~ver:59 ~ver_cmp:(>=) ()
  then ()
  else ()

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

let handle_end_of_candidates (t : t) : unit =
  t.logs.log.str "End of candidates";
  t.webrtc_stuff <- { t.webrtc_stuff with ice_done = true };
  if t.webrtc_stuff.trickle
  then
    (* Notify end of candidates *)
    let candidate = Js.Unsafe.obj [|"completed", Any.of_bool true|] in
    send_trickle_candidate t candidate
  else
    (* No trickle, time to send the complete SDP (including all candidates) *)
    send_sdp t

let on_ice_candidate (t : t) (e : _RTCPeerConnectionIceEvent Js.t) : bool Js.t =
  let eoc = Js.string "endOfCandidates" in
  begin match get_browser (), Js.Opt.to_option e##.candidate with
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
  end;
  Js._true

let on_track (t : t) (e : _RTCTrackEvent Js.t) : bool Js.t =
  t.logs.log.str "Handling Remote Track";
  t.logs.debug.prn e;
  match Js.to_array e##.streams with
  | [||] -> Js._true
  | arr ->
     let (stream : mediaStream Js.t) = arr.(0) in
     Option.iter (fun f -> f stream) t.callbacks.on_remote_stream;
     if Js.Optdef.test (Js.Unsafe.coerce e)##.track
        && Js.Optdef.test (Js.Unsafe.coerce e)##.track##.onended
     then (
       t.logs.log.str_2 "Adding onended callback to track:" e##.track;
       let onended = fun (e : mediaStreamTrack Dom.event Js.t) ->
         t.logs.log.str_2 "Remote track removed:" e;
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
  t.logs.log.str
  @@ Printf.sprintf
       "Preparing local SDP and gathering candidates (trickle=%b)"
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
  t.logs.log.str "Creating PeerConnection";
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
  if Option.is_none config.pc
  then (init_pc t @@ create_pc t);
  let add_tracks =
    match config.local_stream, media.update, config.stream_external with
    | None, _, _ | _, false, _ | _, _, true ->
       t.webrtc_stuff <- { config with local_stream = stream };
       true
    | Some my_stream, _, _ ->
       (* We only need to update the existing stream *)
       begin match Option.flat_map (fun x -> array_get x##getAudioTracks 0) stream with
       | None -> ()
       | Some track ->
          let ({ update; add_audio; replace_audio; _ } : Media.t) = media in
          if ((not update && is_audio_send_enabled ~media ())
              || (update && (add_audio || replace_audio)))
          then update_audio_stream ~replace_audio my_stream track
       end;
       begin match Option.flat_map (fun x -> array_get x##getVideoTracks 0) stream with
       | None -> ()
       | Some track ->
          let ({ update; add_video; replace_video; _ } : Media.t) = media in
          if ((not update && is_video_send_enabled ~media ())
              || (update && (add_video || replace_video)))
          then update_video_stream ~replace_video my_stream track
       end;
       true in
  begin match add_tracks, stream, t.webrtc_stuff.pc with
  | true, Some stream, Some pc ->
     t.logs.log.str "Adding local stream";
     let tracks = stream##getTracks in
     let cb = fun (track : mediaStreamTrack Js.t) _ _ ->
       t.logs.log.str_2 "Adding local track:" track;
       pc##addTrack track stream in
     tracks##forEach (Js.wrap_callback cb)
  | _ -> ()
  end;
  (* Any data channel to create? *)
  begin match is_data_enabled ~media t.logs, t.webrtc_stuff.pc with
  | true, Some pc ->
     t.logs.log.str "Creating data channel";
     let on_message = fun e ->
       t.logs.log.str_2 "Received message on data channel:" e##.data;
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
       t.logs.err.str_2 "Got error on data channel:" e;
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

let send_sdp (t : t) =
  let config = t.webrtc_stuff in
  t.logs.log.str "Sending offer/answer SDP...";
  match config.local_sdp with
  | None ->
     t.logs.warn.str "Local SDP instance is invalid, not sending anything...";
  | Some sdp -> ()

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
  t.logs.log.str (Printf.sprintf "Sending string on data channel: %s" text);
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

