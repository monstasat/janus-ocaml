open Js_of_ocaml

exception Not_created of string

type media_stream

(** Janus plugin handler **)
module Plugin : sig

  (* Types *)

  type e = string

  (** Possible plugin types **)
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

  (** Possible video representations for WebRTC ANSWER/OFFER **)
  type media_video =
    | Bool of bool
    | Resolution of ([`Lowres | `Stdres | `Hires] * [`Wide | `Square ])
    | Screen
    | Device of (int * int * int)

  (** Possible audio representation for WebRTC ANSWER/OFFER **)
  type media_audio =
    | Bool of bool
    | Device of int

  (** Media properties for WebRTC ANSWER/OFFER **)
  type media_props =
    { audio_send : bool option
    ; audio_recv : bool option
    ; audio : media_audio option
    ; video_send : bool option
    ; video_recv : bool option
    ; video : media_video option
    ; data : bool option
    ; fail_if_no_video : bool option
    ; fail_if_no_audio : bool option
    ; screen_rate : int option
    }

  (** Possible plugin response to a message sent **)
  type 'a send_response =
    | Error of int option * string
    | Empty
    | Data of 'a

  (* Helper functions *)

  val parse_sync_response :
    string -> (* plugin name *)
    ('a -> ('b, string) Result.result) -> (* parser function called if data is present *)
    'd send_response -> (* typed response *)
    ('b, string) Result.result (* returned result *)

  val parse_async_response : 'a send_response -> (unit,string) Result.result

  val data_or_error : 'a Js.t Js.optdef -> 'a Js.t send_response

  (* Plugin functions *)

  class type t =
    object
      (** Get plugin id **)
      method id : int64

      (** Get plugin name (with 'janus.plugin.' prefix) **)
      method name : string

      (** Send a message to plugin **)
      method send :
        ?jsep:'a -> (* jsep *)
        'b -> (* plugin-specific request *)
        ('b -> Jsobj.t) -> (* fn converting request to message params *)
        ('c -> 'b -> ('d,string) Result.result) -> (* fn converting js response plugin-specific type *)
        ('d, string) Lwt_result.t

      (** Ask Janus to create a WebRTC compliant ANSWER
      Arguments:
      * media - tells Janus about the media the client is interested in
                and whether client is going to send/receive any of them.
                Default - video and audio enabled in both directions,
                data channels disabled.
      * trickle - whether to use Trickle ICE (default true)
      * jsep - session description sent by the plugin (received in onmessage callback) as its OFFER
      **)
      method create_answer : ?trickle:bool ->
                             media_props ->
                             Js.json Js.t ->
                             (Js.json Js.t, string) Lwt_result.t

      (** Ask Janus to create a WebRTC compliant OFFER
      Arguments:
      same as in 'create_answer', but without jsep
       **)
      method create_offer : ?trickle:bool ->
                            media_props ->
                            (unit, string) Lwt_result.t

      (** Ask Janus to handle an incoming WebRTC complian session description.
      Arguments:
      jsep - session description sent by the plugin (received in onmessage callback) as its ANSWER
       **)
      method handle_remote_jsep : Js.json Js.t ->
                                  (unit, string) Lwt_result.t

      (** Sends DTMF tone on the PeerConnection.
      Arguments:
      * plugin handle
      * DTMF string
      * DTMF duration (default 500)
      * DTMF gap (default 50)
      **)
      method dtmf : ?duration:int ->
                    ?gap:int ->
                    string ->
                    (unit, string) Lwt_result.t

      (** Sends data through the Data Channel, if available.
      Arguments:
      * plugin handle
      * data string
      **)
      method data : string -> (unit, string) Lwt_result.t

      (**
     Gets a verbose description of the currently received stream bitrate
     NOTE: can contain string with error instead of string with bitrate
       **)
      method get_bitrate : unit -> string

      (**
     Tells Janus to close PeerConnection.
     If argument is 'true', then a 'hangup' Janus API request is sent to Janus as well.
     (disabled by detaefault, Janus can usually figure this out via DTLS alerts and the like
     but it may be useful to enable it sometimes)
       **)
      method hangup : ?send_request:bool -> unit -> unit

      (**
     Detaches from the plugin and destroys the handle, tearing down the related PeerConnection
     FIXME: after that plugin handle will become null or undefined, how to handle this case?
       **)
      method detach : unit -> (unit, string) Lwt_result.t

    end

end

module Session' : sig

  type t

  val id : t -> int64

  val server : t -> Uri.t

  val servers : t -> Uri.t list

  val ice_servers : t -> Rtc_peer_connection.ICE.t list

  val ipv6 : t -> bool

  val with_credentials : t -> bool

  val max_poll_events : t -> int

  val token : t -> string option

  val apisecret : t -> string option

  val destroy_on_unload : t -> bool

  val keep_alive_period : t -> int

  val long_poll_timeout : t -> int

  val connected : t -> bool

  val reconnect :
    ?ice_servers:Rtc_peer_connection.ICE.t list ->
    ?ipv6:bool ->
    ?with_credentials:bool ->
    ?max_poll_events:int ->
    ?token:string ->
    ?apisecret:string ->
    ?destroy_on_unload:bool ->
    ?keep_alive_period:int ->
    ?long_poll_timeout:int ->
    server:Uri.t * Uri.t list ->
    t ->
    (t, string) Lwt_result.t

  val destroy :
    ?async_request:bool ->
    ?notify_destroyed:bool ->
    t ->
    (unit, string) Lwt_result.t

end

(** Janus session instance **)
module Session : sig

  (* Types *)

  type e =
    | Err of string
    | Destroyed

  (** Types of JSEP **)
  type jsep =
    | Offer of Js.json Js.t
    | Answer of Js.json Js.t
    | Unknown of Js.json Js.t

  class type t =
    object
      method id : int64
      method server : string
      method connected : bool
      method destroy : unit -> unit
      method attach :
               typ:Plugin.typ ->
               ?opaque_id:string ->
               ?on_local_stream:(media_stream Js.t -> unit) ->
               ?on_remote_stream:(media_stream Js.t -> unit) ->
               ?on_message:(Plugin.t -> Js.json Js.t -> unit) ->
               ?on_jsep:(Plugin.t -> jsep -> unit) ->
               ?consent_dialog:(bool -> unit) ->
               ?webrtc_state:(bool -> unit) ->
               ?ice_state:(string -> unit) ->
               ?media_state:((string * bool) -> unit) ->
               ?slow_link:(bool -> unit) ->
               ?on_cleanup:(unit -> unit) ->
               ?detached:(unit -> unit) ->
               unit ->
               (Plugin.t * Plugin.e React.event) Lwt.t

    end

  (** Create Janus session **)
  val create :
    server:[`One of string | `Many of string list] ->
    ?ice_servers:string list ->
    ?ipv6:bool ->
    ?with_credentials:bool ->
    ?max_poll_events:int ->
    ?destroy_on_unload:bool ->
    ?token:string ->
    ?apisecret:string ->
    unit ->
    (t * e React.event) Lwt.t

end

val is_webrtc_supported : unit -> bool

(** Helper method to attach a MediaStream to a html video element
    Arguments:
    element id - id of a Dom element to attach a stream to
    stream     - MediaStream object *)
val attach_media_stream : #Dom_html.mediaElement Js.t -> media_stream Js.t -> unit

val reattach_media_stream : from:#Dom_html.mediaElement Js.t ->
                            #Dom_html.mediaElement Js.t ->
                            unit

type t

(** Initialize Janus **)
val init : ?log_level:Logs.level -> unit -> unit Lwt.t



