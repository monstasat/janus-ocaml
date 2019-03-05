open Adapter

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

let is_data_enabled ?(media : t option) () : bool =
  match get_browser () with
  | "edge" ->
     Log.ign_warning "Edge doen't support data channels yet";
     false
  | _ ->
     match media with
     | None | Some { data = None; _ } -> false (* Default *)
     | Some { data = Some Bool x; _ } -> x
     | Some { data = Some Options _; _ } -> true

let is_audio_send_enabled ?(media : t option) () : bool =
  match media with
  | None -> true (* Default *)
  | Some (media : t) ->
     match media.audio, media.audio_send with
     | Some Bool false, _ -> false (* Generic audio has precedence *)
     | _, None -> true (* Default *)
     | _, Some x -> x

let is_audio_recv_enabled ?(media : t option) () : bool =
  match media with
  | None -> true (* Default *)
  | Some (media : t) ->
     match media.audio, media.audio_recv with
     | Some Bool false, _ -> false (* Generic audio has precedence *)
     | _, None -> true (* default *)
     | _, Some x -> x

let is_video_send_enabled ?(media : t option) () : bool =
  match media with
  | None -> true
  | Some (media : t) ->
     match media.video, media.video_send with
     | Some Bool false, _ -> false
     | _, None -> true
     | _, Some x -> x

let is_video_recv_enabled ?(media : t option) () : bool =
  match media with
  | None -> true (* Default *)
  | Some (media : t) ->
     match media.video, media.video_recv with
     | Some Bool false, _ -> false (* Generic video has precedence *)
     | _, None -> true (* default *)
     | _, Some x -> x
