open Js_of_ocaml
open Webrtc

type upd = Add | Remove | Replace

type send =
  [ `Bool of bool
  | `Constraints of mediaTrackConstraints Js.t
  ]

type video =
  [ send
  | `Resolution of resolution
  | `Screen of int option (* screenshare frame rate *)
  | `Window of int option (* screenshare frame rate *)
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

let resolution_to_video_size : resolution -> int * int = function
  | `Low -> 320, 240 (* Small resolution, 4:3 *)
  | `Low_wide -> 320, 180 (* Small resolution, 16:9 *)
  | `SD -> 640, 480 (* Normal resolution, 4:3 *)
  | `SD_wide -> 640, 360 (* Normal resolution, 16:9 *)
  | `HD -> 1280, 720 (* High (HD) resolution is only 16:9 *)
  | `FullHD -> 1920, 1080 (* Full HD resolution is only 16:9 *)
  | `UHD -> 3840, 2160 (* 4K resolution is only 16:9 *)

type audio = send

type 'a track =
  { fail_if_not_available : bool
  ; update : upd option
  ; source : 'a
  }

type data =
  [ `Bool of bool
  | `Init of _RTCDataChannelInit Js.t
  ]

type t =
  { audio : audio track
  ; video : video track
  }

type source =
  [ `Stream of mediaStream Js.t
  | `Media of t
  ]

let make_audio ?(fail_if_not_available = false)
      ?(update : upd option)
      ?(source = `Bool true)
      () : audio track =
  { fail_if_not_available
  ; update
  ; source
  }

let make_video ?(fail_if_not_available = false)
      ?(update : upd option)
      ?(source = `Bool true)
      () : video track =
  { fail_if_not_available
  ; update
  ; source
  }

let make ?audio ?video () : t =
  let video = match video with
    | Some x -> x
    | None -> make_video () in
  let audio = match audio with
    | Some x -> x
    | None -> make_audio () in
  { video
  ; audio
  }

let is_track_send_enabled (track : 'a track) : bool =
  match track.source with
  | `Bool x -> x
  | _ -> true

let is_track_send_required (track : 'a track) : bool =
  match track.source with
  | `Bool false -> false
  | _ -> track.fail_if_not_available
