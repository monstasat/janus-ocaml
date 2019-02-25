open Js_of_ocaml
open Utils
open Types
open Media_stream

type callbacks =
  { on_local_stream : unit -> unit
  ; on_remote_stream : unit -> unit
  ; on_message : unit -> unit
  ; on_jsep : unit -> unit
  ; consent_dialog : bool -> unit
  ; ice_state : string -> unit
  ; webrtc_state : bool -> unit
  ; media_state : (string * bool) -> unit
  ; slow_link : bool -> unit
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
    ; pc : unit option
    ; data_channel : unit option
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
  ; logs : logs
  ; mutable webrtc_stuff : Webrtc_stuff.t
  ; mutable detached : bool
  }

let is_audio_send_enabled ?(media : Media.t option) (t : t) : bool =
  match media with
  | None -> true (* Default *)
  | Some (media : Media.t) ->
     match media.audio, media.audio_send with
     | Some Bool false, _ -> false (* Generic audio has precedence *)
     | _, None -> true (* Default *)
     | _, Some x -> x

let is_video_send_enabled ?(media : Media.t option) (t : t) : bool =
  match media with
  | None -> true
  | Some (media : Media.t) ->
     match media.video, media.video_send with
     | Some Bool false, _ -> false
     | _, None -> true
     | _, Some x -> x

let prepare_webrtc ?jsep
      ?(stream : mediaStream Js.t option)
      (media : Media.t)
      (t : t) =
  let config = t.webrtc_stuff in
  (* Are we updating a session ? *)
  match config.pc with
  | None -> ()
  | Some pc ->
     t.logs.info (fun m -> m "Updating existing media session");
     ()

let update_audio_stream ~(replace_audio : bool)
      (stream : mediaStream Js.t)
      (track : mediaStreamTrack Js.t) =
  stream##addTrack track;
  let (details : Adapter.browser_details) = Adapter.browser_details () in
  match replace_audio, details.browser, details.version with
  | true, "firefox", v | true, "chrome", v when v >= 72 ->
     ()
  | _, "firefox", v when v >= 59 ->
     ()
  | _ -> ()

let update_video_stream ~(replace_video : bool)
      (stream : mediaStream Js.t)
      (track : mediaStreamTrack Js.t) =
  stream##addTrack track;
  let details = Adapter.browser_details () in
  match replace_video, details.browser, details.version with
  | true, "firefox", v | true, "chrome", v when v >= 72 ->
     ()
  | _, "firefox", v when v >= 59 ->
     ()
  | _ -> ()

let streams_done (t : t)
      jsep
      ?(stream : mediaStream Js.t option)
      (media : Media.t) =
  let config = t.webrtc_stuff in
  (* We're now capturing the new stream:
     check if we're updating or it's a new thing *)
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
          if ((not update && is_audio_send_enabled ~media t)
              || (update && (add_audio || replace_audio)))
          then update_audio_stream ~replace_audio my_stream track
       end;
       begin match Utils.array_get stream##getVideoTracks 0 with
       | None -> ()
       | Some track ->
          let ({ update; add_video; replace_video; _ } : Media.t) = media in
          if ((not update && is_video_send_enabled ~media t)
              || (update && (add_video || replace_video)))
          then update_video_stream ~replace_video my_stream track
       end;
       true in
  ()

let send_sdp (t : t) =
  let config = t.webrtc_stuff in
  t.logs.info (fun m -> m "Sending offer/answer SDP...");
  match config.local_sdp with
  | None ->
     t.logs.warn (fun m ->
         m "Local SDP instance is invalid, not sending anything...");
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
  t.logs.info (fun m -> m "Sending string on data channel: %s" text);
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

