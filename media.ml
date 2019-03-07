open Adapter

type dir =
  { send : bool
  ; recv : bool
  }

type upd = Add | Remove | Replace

type common_src =
  [ `Bool of bool
  | `Dir of dir
  ]

type video =
  [ common_src
  | `Resolution of resolution
  | `Screen of int option (* screenshare frame rate *)
  | `Window of int option (* screenshare frame rate *)
  | `Device of video_device
  ]
and resolution =
  [ `Low
  | `Low_wide
  | `SD
  | `SD_wide
  | `HD
  | `FullHD
  | `UHD
  ]
and video_device =
  { device_id : int
  ; width : int
  ; height : int
  }

let resolution_to_video_size : resolution -> int * int = function
  | `Low -> 320, 240 (* Small resolution, 4:3 *)
  | `Low_wide -> 320, 180 (* Small resolution, 16:9 *)
  | `SD -> 640, 480 (* Normal resolution, 4:3 *)
  | `SD_wide -> 640, 360 (* Normal resolution, 16:9 *)
  | `HD -> 1280, 720 (* High (HD) resolution is only 16:9 *)
  | `FullHD -> 1920, 1080 (* Full HD resolution is only 16:9 *)
  | `UHD -> 3840, 2160 (* 4K resolution is only 16:9 *)

type audio =
  [ common_src
  | `Device of audio_device
  ]
and audio_device =
  { device_id : int
  }

type track =
  { fail_if_not_available : bool
  ; update : upd option
  ; typ : track_type
  }
and track_type =
  | Video of video
  | Audio of audio

type data =
  [ `Bool of bool
  | `Options of data_options
  ]
and data_options =
  { ordered : bool option
  ; max_packet_life_time : int option
  ; max_retransmits : int option
  ; protocol : string option
  ; negotiated : bool option
  ; id : int option
  }

let make_audio ?(fail_if_not_available = false) ?(update : upd option)
      (src : audio) : track =
  { fail_if_not_available
  ; update
  ; typ = Audio src
  }

let make_video ?(fail_if_not_available = false) ?(update : upd option)
      (src : video) : track =
  { fail_if_not_available
  ; update
  ; typ = Video src
  }

type t =
  { audio : track
  ; video : track
  ; data : data
  }

type t_ext =
  { update : bool
  ; keep_audio : bool
  ; keep_video : bool
  ; media : t
  }

let is_data_enabled ?(media : t option) () : bool =
  match get_browser () with
  | "edge" ->
     Log.ign_warning "Edge doen't support data channels yet";
     false
  | _ ->
     match media with
     | None -> false
     | Some (media : t) ->
        match media.data with
        | `Bool x -> x
        | `Options _ -> true

let is_track_send_enabled (track : track) : bool =
  match track.typ with
  | Video `Bool x | Audio `Bool x -> x
  | Video `Dir x | Audio `Dir x -> x.send
  | _ -> true

let is_track_recv_enabled (track : track) : bool =
  match track.typ with
  | Video `Bool x | Audio `Bool x -> x
  | Video `Dir x | Audio `Dir x -> x.recv
  | _ -> true

let should_remove_track (track : track) : bool =
  match track.update with
  | Some Remove -> true
  | None | Some _ -> false
