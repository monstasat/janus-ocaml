open Js_of_ocaml

exception Not_created of string

(** Janus plugin handler **)
module Plugin : sig

  (* Types *)

  (** Plugin handler **)
  type t = Janus.plugin Js.t

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

  (** Get plugin id **)
  val get_id : t -> int64

  (** Get plugin name (with 'janus.plugin.' prefix) **)
  val get_name : t -> string

  (** Send a message to plugin **)
  val send :
    ?jsep:'a -> (* jsep *)
    t -> (* plugin *)
    'b -> (* plugin-specific request *)
    ('b -> string) -> (* fn converting request to string *)
    ('b -> (string * Js.Unsafe.any) array) -> (* fn converting request to message params *)
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
  val create_answer : t -> media_props -> bool option ->
                      Js.json Js.t ->
                      (Js.json Js.t, string) Lwt_result.t

  (** Ask Janus to create a WebRTC compliant OFFER
      Arguments:
      same as in 'create_answer', but without jsep
   **)
  val create_offer  : t ->
                      media_props ->
                      bool option ->
                      (unit, string) Lwt_result.t

  (** Ask Janus to handle an incoming WebRTC complian session description.
      Arguments:
      jsep - session description sent by the plugin (received in onmessage callback) as its ANSWER
   **)
  val handle_remote_jsep : t ->
                           Js.json Js.t ->
                           (unit, string) Lwt_result.t

  (** Sends DTMF tone on the PeerConnection.
      Arguments:
      * plugin handle
      * DTMF string
      * DTMF duration (default 500)
      * DTMF gap (default 50)
   **)
  val dtmf : t ->
             string ->
             int option ->
             int option ->
             (unit, string) Lwt_result.t

  (** Sends data through the Data Channel, if available.
      Arguments:
      * plugin handle
      * data string
   **)
  val data : t -> string -> (unit, string) Lwt_result.t

  (**
     Gets a verbose description of the currently received stream bitrate
     NOTE: can contain string with error instead of string with bitrate
   **)
  val get_bitrate : t -> string

  (**
     Tells Janus to close PeerConnection.
     If argument is 'true', then a 'hangup' Janus API request is sent to Janus as well.
     (disabled by detaefault, Janus can usually figure this out via DTLS alerts and the like
     but it may be useful to enable it sometimes)
   **)
  val hangup : t -> bool -> unit

  (**
     Detaches from the plugin and destroys the handle, tearing down the related PeerConnection
     FIXME: after that plugin handle will become null or undefined, how to handle this case?
   **)
  val detach : t -> (unit, string) Lwt_result.t

end

(** Janus session instance **)
module Session : sig

  (* Types *)

  (** Janus instance **)
  type t = Janus.janus Js.t

  type e =
    | Err of string
    | Destroyed

  (** Types of JSEP **)
  type jsep =
    | Offer of Js.json Js.t
    | Answer of Js.json Js.t
    | Unknown of Js.json Js.t

  (* Session functions *)

  (** Returns the address of the gateway **)
  val get_server : t -> string

  (** Returns 'true' if the Janus session is connected to the gateway **)
  val is_connected : t -> bool

  (** Returns the unique gateway session identifier **)
  val get_session_id : t -> int64

  (** Attach plugin to Janus session.
      More than one plugin (even of the same type) can be attached to one session
   **)
  val attach :
    session:t -> typ:Plugin.typ ->
    ?opaque_id:string ->
    ?on_local_stream:(Janus.media_stream Js.t -> unit) ->
    ?on_remote_stream:(Janus.media_stream Js.t -> unit) ->
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

  (** Destroy Janus session and close all the plugin handles and PeerConnections **)
  val destroy : t -> (unit, string) Lwt_result.t

end

(** Available Janus debuggers **)
type debug_token = Trace
                 | Debug
                 | Log
                 | Warn
                 | Error

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
  (Session.t * Session.e React.event) Lwt.t

(** Initialize Janus **)
val init : [`All of bool | `Several of debug_token list ] -> unit Lwt.t



