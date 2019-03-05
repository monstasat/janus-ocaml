open Js_of_ocaml
open Webrtc

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
    ; local_sdp : _RTCSessionDescriptionInit Js.t option
    ; remote_sdp : _RTCSessionDescriptionInit Js.t option
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
