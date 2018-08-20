(** Represents a stream of media content.
    A stream consists of several tracks such as video or audio track.
    https://developer.mozilla.org/en-US/docs/Web/API/MediaStream *)
type media_stream

type js_obj = (string * Js.Unsafe.any) array

(** Plugin handle object API **)
class type plugin =
  object
    method getId : unit -> Js.number Js.t Js.meth
    method getPlugin : unit -> Js.js_string Js.t Js.meth
    method send : js_obj -> unit Js.meth
    method createAnswer : js_obj -> unit Js.meth
    method createOffer : js_obj -> unit Js.meth
    method handleRemoteJsep : js_obj -> unit Js.meth
    method dtmf : js_obj -> unit Js.meth
    method data : js_obj -> unit Js.meth
    method getBitrate : unit -> Js.js_string Js.t Js.meth
    method hangup : bool Js.t -> unit Js.meth
    method detach : js_obj -> unit Js.meth
  end

(** Janus session object API *)
class type janus =
  object
    method getServer : unit -> Js.js_string Js.t Js.meth
    method isConnected : unit -> bool Js.t Js.meth
    method getSessionId : unit -> Js.number Js.t Js.meth
    method attach : js_obj -> unit Js.meth
    method destroy : js_obj -> unit Js.meth
  end

(** Janus initialization *)
val init : js_obj -> unit

(** Create Janus session *)
val create : js_obj -> 'a

(** Helper method to check whether WebRTC is supported by the browser *)
val isWebrtcSupported : unit -> bool

(** Helper method to attach a MediaStream to a html video element
    Arguments:
    element id - id of a Dom element to attach a stream to
    stream     - MediaStream object *)
val attachMediaStream : #Dom_html.element Js.t -> media_stream Js.t -> unit
