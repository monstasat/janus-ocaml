open Js_of_ocaml
open Utils

module Any = struct
  type t = Js.Unsafe.any
  let _of (x : 'a) : t = Js.Unsafe.inject x
  let of_list (f : 'a -> t) (x : 'a list) : t = _of @@ Js.array @@ Array.of_list @@ List.map f x
  let of_int (x : int) : t = _of x
  let of_bool (x : bool) : t = _of @@ Js.bool x
  let of_string (x : string) : t = _of @@ Js.string x
  let of_option (f : 'a -> 'b) (x : 'a option) : t = _of @@ Js.Optdef.(map (option x) f)
  let of_string_opt (x : string option) : t = of_option Js.string x
  let of_int_opt (x : int option) : t = of_option (fun x -> x) x
  let of_bool_opt (x : bool option) : t = of_option Js.bool x
end

module Jsobj = struct
  type t = (string * Any.t) array
  let empty = [||]
  let concat = Array.concat
  let append = Array.append
  let make = Array.of_list
  let singleton k v = [|k, v|]
  let ( @ ) = append
end

let int_of_number x = int_of_float @@ Js.float_of_number x

let parse_ok_response ok =
  if String.equal (Js.to_string ok) "ok"
  then Ok ()
  else Error "Bad ok response"

module Mp_base = struct

  type t =
    { id : int option
    ; name : string option
    ; description : string option
    ; is_private : bool
    ; audio : bool
    ; video : bool
    ; data : bool
    }

  let to_js_obj (x : t) : Jsobj.t =
    [| ("id", Any.of_int_opt x.id)
     ; ("name", Any.of_string_opt x.name)
     ; ("description", Any.of_string_opt x.description)
     ; ("is_private", Any.of_bool x.is_private)
     ; ("audio", Any.of_bool x.audio)
     ; ("video", Any.of_bool x.video)
     ; ("data", Any.of_bool x.data)
    |]

end

(* Parameters for rtp mountpoint*)
module Mp_rtp = struct

  type audio =
    { audiomcast : string option
    ; audioport : int
    ; audiopt : int
    ; audiortpmap : string
    ; audiofmtp : string option
    ; audioiface : string option
    }

  type video =
    { videomcast : string option
    ; videoport : int
    ; videopt : int
    ; videortpmap : string
    ; videofmtp : string option
    ; videoiface : string option
    ; videobufferkf : bool option
    }

  type data =
    { dataport : int
    ; databuffermsg : bool option
    ; dataiface : string option
    }

  type t =
    { base : Mp_base.t
    ; audio : audio option
    ; video : video option
    ; data : data option
    }

  let to_js_obj (x : t) : Jsobj.t =
    let base = Mp_base.to_js_obj x.base in
    let audio = match x.audio with
      | None -> Jsobj.empty
      | Some aud ->
         [| ("audiomcast", Any.of_string_opt aud.audiomcast)
          ; ("audioport", Any.of_int aud.audioport)
          ; ("audiopt", Any.of_int aud.audiopt)
          ; ("audiortpmap", Any.of_string aud.audiortpmap)
          ; ("audiofmtp", Any.of_string_opt aud.audiofmtp)
          ; ("audioiface", Any.of_string_opt aud.audioiface)
         |] in
    let video = match x.video with
      | None -> Jsobj.empty
      | Some vid ->
         [| ("videomcast", Any.of_string_opt vid.videomcast)
          ; ("videoport", Any.of_int vid.videoport)
          ; ("videopt", Any.of_int vid.videopt)
          ; ("videortpmap", Any.of_string vid.videortpmap)
          ; ("videofmtp", Any.of_string_opt vid.videofmtp)
          ; ("videoiface", Any.of_string_opt vid.videoiface)
          ; ("videobufferkf", Any.of_bool_opt vid.videobufferkf)
         |] in
    let data = match x.data with
      | None -> Jsobj.empty
      | Some data ->
         [| ("dataport", Any.of_int data.dataport)
          ; ("databuffermsg", Any.of_bool_opt data.databuffermsg)
          ; ("dataiface", Any.of_string_opt data.dataiface)
         |] in
    Jsobj.concat [base; audio; video; data]

end

(* Parameters for live mountpoint *)
module Mp_live = struct

  type t =
    { base : Mp_base.t option
    ; filename : string
    }

  let to_js_obj (x : t) : Jsobj.t =
    let base = match x.base with
      | None -> Jsobj.empty
      | Some x -> Mp_base.to_js_obj x in
    [|("filename", Any.of_string x.filename)|]
    |> Jsobj.append base

end

(* Parameters for ondemand mountpoint *)
module Mp_ondemand = struct

  type t = Mp_live.t

  let to_js_obj (x : t) : Jsobj.t =
    Mp_live.to_js_obj x

end

(* Parameters for rtps mountpoint *)
module Mp_rtsp = struct

  type t =
    { base : Mp_base.t option
    ; url : string option
    ; rtsp_user : string option
    ; rtsp_pwd : string option
    ; rtspiface : string option
    }

  let to_js_obj (x : t) : Jsobj.t =
    let base = match x.base with
      | None -> Jsobj.empty
      | Some x -> Mp_base.to_js_obj x in
    [| ("url", Any.of_string_opt x.url)
     ; ("rtsp_user", Any.of_string_opt x.rtsp_user)
     ; ("rtsp_pwd", Any.of_string_opt x.rtsp_pwd)
     ; ("rtspiface", Any.of_string_opt x.rtspiface)
    |]
    |> Jsobj.append base

end

module Mp_list = struct

  type t = unit

  type r =
    { id : int option
    ; type_ : string option
    ; description : string option
    ; video_age_ms : int option
    ; audio_age_ms : int option
    }

  let to_js_obj (_ : t) : Jsobj.t = Jsobj.empty

  let of_js_obj o =
    Array.map (fun el ->
        let el = Js.Unsafe.coerce el in
        { id  = Js.Optdef.to_option el##.id |> Option.map int_of_number
        ; type_ = Js.Optdef.to_option el##.type_ |> Option.map Js.to_string
        ; description = Js.Optdef.to_option el##.description |> Option.map Js.to_string
        ; video_age_ms = Js.Optdef.to_option el##.video_age_ms |> Option.map int_of_number
        ; audio_age_ms = Js.Optdef.to_option el##.audio_age_ms |> Option.map int_of_number
        }) o
    |> Array.to_list
    |> (fun x -> Ok x)

end

module Mp_info = struct

  type t = int

  type r =
    { id : int option
    ; type_ : string option
    ; description : string option
    ; video_age_ms : int option
    ; audio_age_ms : int option
    ; data_age_ms : int option
    ; video : string option
    ; audio : string option
    ; data : string option
    }

  let to_js_obj (id : int) : Jsobj.t =
    [|("id", Any.of_int id)|]

  let of_js_obj o =
    let o = Js.Unsafe.coerce o in
    Js.Optdef.(
      Ok { id = Option.map int_of_number @@ to_option o##.id
         ; type_ = Option.map Js.to_string @@ to_option o##.type_
         ; description = Option.map Js.to_string @@ to_option o##.description
         ; video_age_ms = Option.map int_of_number @@ to_option o##.video_age_ms
         ; audio_age_ms = Option.map int_of_number @@ to_option o##.audio_age_ms
         ; data_age_ms = Option.map int_of_number @@ to_option o##.data_age_msg
         ; video = Option.map Js.to_string @@ to_option o##.video
         ; audio = Option.map Js.to_string @@ to_option o##.audio
         ; data = Option.map Js.to_string @@ to_option o##.data })

end

module Mp_create = struct

  type mp_type =
    | Rtp of Mp_rtp.t
    | Live of Mp_live.t
    | Ondemand of Mp_ondemand.t
    | Rtsp of Mp_rtsp.t

  type t =
    { type_ : mp_type
    ; admin_key : string option
    ; secret : string option
    ; pin : string option
    ; permanent : bool
    }

  type r =
    { id : int option
    ; type_ : string option
    ; description : string option
    ; is_private : bool option
    ; audio_port : int option
    ; video_port : int option
    ; data_port : int option
    }

  let type_to_string = function
    | Rtp _ -> "rtp"
    | Live _ -> "live"
    | Ondemand _ -> "ondemand"
    | Rtsp _ -> "rtsp"

  let to_js_obj (x : t) : Jsobj.t =
    let type_ = match x.type_ with
      | Rtp x -> Mp_rtp.to_js_obj x
      | Live x -> Mp_live.to_js_obj x
      | Rtsp x -> Mp_rtsp.to_js_obj x
      | Ondemand x -> Mp_ondemand.to_js_obj x in
    [| ("type", Any.of_string @@ type_to_string x.type_)
     ; ("admin_key", Any.of_string_opt x.admin_key)
     ; ("secret", Any.of_string_opt x.secret)
     ; ("pin", Any.of_string_opt x.pin)
     ; ("permanent", Any.of_bool x.permanent)
    |]
    |> Jsobj.append type_

  let of_js_obj (o : 'a Js.t) : (r, 'b) result =
    let o = Js.Unsafe.coerce o in
    Js.Optdef.(
      Ok { id = Option.map int_of_number @@ to_option o##.id
         ; type_ = Option.map Js.to_string @@ to_option o##.type_
         ; description = Option.map Js.to_string @@ to_option o##.description
         ; is_private = Option.map Js.to_bool @@ to_option o##.is_private
         ; audio_port = Option.map int_of_number @@ to_option o##.audio_port
         ; video_port = Option.map int_of_number @@ to_option o##.video_port
         ; data_port = Option.map int_of_number @@ to_option o##.data_port })

end

module Mp_destroy = struct

  type t =
    { id : int
    ; secret : string option
    ; permanent : bool option
    }

  type r = int

  let to_js_obj (x : t) : Jsobj.t =
    [| ("id", Js.Unsafe.inject x.id)
     ; ("secret", Any.of_string_opt x.secret)
     ; ("permanent", Any.of_bool_opt x.permanent)
    |]

  let of_js_obj o = Ok (int_of_number o)

end

module Mp_recording = struct

  type recording_action =
    | Start of (string option * string option * string option) (* filenames *)
    | Stop of (bool option * bool option * bool option) (* flags *)

  type t =
    { id : int
    ; action : recording_action
    ; secret : string option
    }

  type r = unit

  let action_to_string : recording_action -> string = function
    | Start _ -> "start"
    | Stop _ -> "stop"

  let to_js_obj (x : t) : Jsobj.t =
    let params = match x.action with
      | Start (a, v, d) ->
         [| ("audio", Any.of_string_opt a)
          ; ("video", Any.of_string_opt v)
          ; ("data", Any.of_string_opt d)
         |]
      | Stop (a, v, d) ->
         [| ("audio", Any.of_bool_opt a)
          ; ("video", Any.of_bool_opt v)
          ; ("data", Any.of_bool_opt d)
         |] in
    [| ("id", Js.Unsafe.inject x.id)
     ; ("action", Any.of_string @@ action_to_string x.action)
     ; ("secret", Any.of_string_opt x.secret)
    |]
    |> Jsobj.append params

  let of_js_obj o = parse_ok_response o

end

module Mp_enable = struct

  type t =
    { id : int
    ; secret : string option
    }

  type r = unit

  let to_js_obj (x : t) : Jsobj.t =
    [| ("id", Any.of_int x.id)
     ; ("secret", Any.of_string_opt x.secret)
    |]

  let of_js_obj o = parse_ok_response o

end

module Mp_disable = struct

  include Mp_enable

end

module Mp_watch = struct

  type t =
    { id : int
    ; secret : string option
    }

  let to_js_obj (x : t) =
    [ ("id", Any.of_int x.id)
    ; ("secret", Any.of_string_opt x.secret )
    ]
    |> Jsobj.make

end

module type Playback = sig

  type t = unit

  val to_js_obj : t -> (string * Js.Unsafe.any) array

end

module Mp_start : Playback = struct

  type t = unit

  let to_js_obj (_ : t) : Jsobj.t = Jsobj.empty

end

module Mp_pause : Playback = Mp_start

module Mp_stop  : Playback = Mp_start

module Mp_switch = struct

  type t = int

  let to_js_obj (id : t) : Jsobj.t =
    [|("id", Any.of_int id)|]

end

type _ request =
  | List : Mp_list.r list request
  | Info : Mp_info.t -> Mp_info.r request
  | Create : Mp_create.t -> Mp_create.r request
  | Destroy : Mp_destroy.t -> Mp_destroy.r request
  | Recording : Mp_recording.t -> Mp_recording.r request
  | Enable : Mp_enable.t -> Mp_enable.r request
  | Disable : Mp_disable.t -> Mp_disable.r request
  | Watch : Mp_watch.t -> unit request
  | Start : unit request
  | Pause : unit request
  | Stop : unit request
  | Switch : Mp_switch.t -> unit request

let request_to_string : type a. a request -> string = function
  | List -> "list"
  | Info _ -> "info"
  | Create _ -> "create"
  | Destroy _ -> "destroy"
  | Recording _ -> "recording"
  | Enable _ -> "enable"
  | Disable _ -> "disable"
  | Watch _ -> "watch"
  | Start -> "start"
  | Pause -> "pause"
  | Stop -> "stop"
  | Switch _ -> "switch"

let request_to_obj : type a. a request -> Jsobj.t =
  fun req ->
  let (params : Jsobj.t) = match req with
    | List -> Mp_list.to_js_obj ()
    | Info x -> Mp_info.to_js_obj x
    | Create x -> Mp_create.to_js_obj x
    | Destroy x -> Mp_destroy.to_js_obj x
    | Recording x -> Mp_recording.to_js_obj x
    | Enable x -> Mp_enable.to_js_obj x
    | Disable x -> Mp_disable.to_js_obj x
    | Watch x -> Mp_watch.to_js_obj x
    | Start -> Mp_start.to_js_obj ()
    | Pause -> Mp_pause.to_js_obj ()
    | Stop -> Mp_stop.to_js_obj ()
    | Switch x -> Mp_switch.to_js_obj x in
  let (request : Jsobj.t) =
    Jsobj.singleton "request" (Any.of_string @@ request_to_string req) in
  let (message : Jsobj.t) =
    Jsobj.(singleton "message" (Any._of @@ Js.Unsafe.obj (request @ params))) in
  Jsobj.(request @ message)

(* let parse_response (type a) response
 *       (request : a request)
 *     : (a, string) result =
 *   let open Plugin in
 *   let plugin_name = "streaming" in
 *   let typed_response = data_or_error response in
 *   begin match request with
 *   (\* sync responses *\)
 *   | List -> parse_sync_response plugin_name Mp_list.of_js_obj typed_response
 *   | Info _ -> parse_sync_response plugin_name Mp_info.of_js_obj typed_response
 *   | Create _ -> parse_sync_response plugin_name Mp_create.of_js_obj typed_response
 *   | Destroy _ -> parse_sync_response plugin_name Mp_destroy.of_js_obj typed_response
 *   | Recording _ -> parse_sync_response plugin_name parse_ok_response typed_response
 *   | Enable _ -> parse_sync_response plugin_name parse_ok_response typed_response
 *   | Disable _ -> parse_sync_response plugin_name parse_ok_response typed_response
 *   (\* async responses *\)
 *   | Watch _ -> parse_async_response typed_response
 *   | Start -> parse_async_response typed_response
 *   | Pause -> parse_async_response typed_response
 *   | Stop -> parse_async_response typed_response
 *   | Switch _ -> parse_async_response typed_response
 *   end *)

(* let default_media_props =
 *   let open Plugin in
 *   { audio_send = Some false
 *   ; audio_recv = None
 *   ; audio = None
 *   ; video_send = Some false
 *   ; video_recv = None
 *   ; video = None
 *   ; data = None
 *   ; fail_if_no_video = None
 *   ; fail_if_no_audio = None
 *   ; screen_rate = None
 *   } *)
