open Js_of_ocaml
open Webrtc

module Streaming = Janus_streaming

module Webrtc = Webrtc

module Media : sig

  type upd =
    | Add
    | Remove
    | Replace

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

  type audio = send

  type 'a track =
    { fail_if_not_available : bool
    ; update : upd option
    ; send : 'a
    ; recv : bool
    }

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

  type t =
    { audio : audio track
    ; video : video track
    ; data : data
    }

  type source =
    [ `Stream of mediaStream Js.t
    | `Create of t
    ]

  val make_audio : ?fail_if_not_available:bool ->
                   ?update:upd ->
                   ?recv:bool ->
                   ?send:audio ->
                   unit ->
                   audio track
  val make_video : ?fail_if_not_available:bool ->
                   ?update:upd ->
                   ?recv:bool ->
                   ?send:video ->
                   unit ->
                   video track

  val make : ?audio:audio track ->
             ?video:video track ->
             ?data:data ->
             unit ->
             t

end

module Plugin : sig

  type t

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

  type ice_transport_policy =
    | All
    | Public
    | Relay

  type bundle_policy =
    | Balanced
    | Max_compact
    | Max_bundle

  type ice_connection_state =
    | New
    | Checking
    | Connected
    | Completed
    | Failed
    | Disconnected
    | Closed
    | Unknown

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

  val ice_connection_state_to_string : ice_connection_state -> string

  val id : t -> int

  val typ : t -> typ

  val send_message :
    ?message:'a Js.t ->
    ?jsep:_RTCSessionDescription Js.t ->
    t ->
    ('a Js.t option, string) Lwt_result.t

  val create_offer :
    ?simulcast:bool ->
    ?trickle:bool ->
    Media.source ->
    t ->
    (_RTCSessionDescription Js.t option, string) Lwt_result.t

  val create_answer :
    ?simulcast:bool ->
    ?trickle:bool ->
    jsep:_RTCSessionDescriptionInit Js.t ->
    Media.source ->
    t ->
    (_RTCSessionDescription Js.t option, string) Lwt_result.t

  val handle_remote_jsep :
    _RTCSessionDescriptionInit Js.t ->
    t ->
    (unit, string) Lwt_result.t

  val hangup : ?request:bool -> t -> unit

  val detach : ?async:bool -> t -> (unit, string) Lwt_result.t

end

module Session : sig

  type t

  val id : t -> int

  val server : t -> string

  val ice_servers : t -> _RTCIceServer Js.t list

  val ipv6 : t -> bool

  val with_credentials : t -> bool

  val max_poll_events : t -> int

  val token : t -> string option

  val apisecret : t -> string option

  val destroy_on_unload : t -> bool

  val keep_alive_period : t -> int

  val long_poll_timeout : t -> int

  val connected : t -> bool

  val attach_plugin :
    ?opaque_id:string ->
    ?token:string ->
    ?rtc_constraints:(string * Js.Unsafe.any) array list ->
    ?on_local_stream:(mediaStream Js.t -> unit) ->
    ?on_remote_stream:(mediaStream Js.t -> unit) ->
    ?on_message:(?jsep:_RTCSessionDescriptionInit Js.t ->
                 'a Js.t ->
                 Plugin.t ->
                 unit) ->
    ?on_consent_dialog:(bool -> unit) ->
    ?on_ice_state:(Plugin.ice_connection_state -> unit) ->
    ?on_webrtc_state:(Plugin.webrtc_state -> unit) ->
    ?on_media_state:(Plugin.media_state -> unit) ->
    ?on_slow_link:(Plugin.slow_link -> unit) ->
    ?on_data:(string -> unit) ->
    ?on_data_open:(unit -> unit) ->
    ?on_data_error:(< > Js.t -> unit) ->
    ?on_cleanup:(unit -> unit) ->
    ?on_detached:(unit -> unit) ->
    typ:Plugin.typ ->
    t ->
    (Plugin.t, string) Lwt_result.t

  val reconnect : t -> (unit, string) Lwt_result.t

  val destroy :
    ?async:bool ->
    ?notify_destroyed:bool ->
    t ->
    (unit, string) Lwt_result.t

end

type t

val is_webrtc_supported : unit -> bool

val attach_media_stream :
  #Dom_html.mediaElement Js.t ->
  mediaStream Js.t ->
  unit

val reattach_media_stream :
  from:#Dom_html.mediaElement Js.t ->
  #Dom_html.mediaElement Js.t ->
  unit

val create_session :
  ?ice_servers:_RTCIceServer Js.t list ->
  ?ipv6:bool ->
  ?with_credentials:bool ->
  ?max_poll_events:int ->
  ?token:string ->
  ?apisecret:string ->
  ?ice_transport_policy:Plugin.ice_transport_policy ->
  ?bundle_policy:Plugin.bundle_policy ->
  ?destroy_on_unload:bool ->
  ?keep_alive_period:int ->
  ?long_poll_timeout:int ->
  ?on_destroyed:(unit -> unit) ->
  ?on_error:(string -> unit) ->
  ?aux_servers:string list ->
  server:string ->
  t ->
  (Session.t, string) Lwt_result.t

val create : ?log_level:Lwt_log_js.level -> unit -> t
