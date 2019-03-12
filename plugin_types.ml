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

type volume =
  { value : unit option
  ; timer : unit option
  }

type bitrate_stuff =
  { audio : track_bitrate
  ; video : track_bitrate
  ; timer : Dom_html.interval_id option
  }
and track_bitrate =
  { value : int option
  ; bytes : int option
  ; timestamp : float option
  }

module Webrtc_stuff = struct

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
  ; mutable bitrate : bitrate_stuff
  ; mutable volume : volume
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
  ; on_data : 'a. ('a Js.t -> unit) option
  ; on_data_open : (unit -> unit) option
  ; on_data_close : (unit -> unit) option
  ; on_data_error : (_RTCError Js.t -> unit) option
  ; on_cleanup : (unit -> unit) option
  ; on_detached : (unit -> unit) option
  ; rm_from_session : int -> unit
  }
