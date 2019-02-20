(** Represents a stream of media content.
    A stream consists of several tracks such as video or audio track.
    https://developer.mozilla.org/en-US/docs/Web/API/MediaStream *)

open Js_of_ocaml

type media_stream

(** Plugin handle object API **)
class type plugin =
  object
    method getId : unit -> Js.number Js.t Js.meth
    method getPlugin : unit -> Js.js_string Js.t Js.meth
    method send : Jsobj.t_js -> unit Js.meth
    method createAnswer : Jsobj.t_js -> unit Js.meth
    method createOffer : Jsobj.t_js -> unit Js.meth
    method handleRemoteJsep : Jsobj.t_js -> unit Js.meth
    method dtmf : Jsobj.t_js -> unit Js.meth
    method data : Jsobj.t_js -> unit Js.meth
    method getBitrate : unit -> Js.js_string Js.t Js.meth
    method hangup : bool Js.t -> unit Js.meth
    method detach : Jsobj.t_js -> unit Js.meth
  end

(** Janus session object API *)
class type janus =
  object
    method getServer : unit -> Js.js_string Js.t Js.meth
    method isConnected : unit -> bool Js.t Js.meth
    method getSessionId : unit -> Js.number Js.t Js.meth
    method attach : Jsobj.t_js -> unit Js.meth
    method destroy : Jsobj.t_js -> unit Js.meth
  end

(** Janus initialization *)
val init : Jsobj.t -> unit

(** Create Janus session *)
val create : Jsobj.t -> 'a

(** Helper method to check whether WebRTC is supported by the browser *)
val isWebrtcSupported : unit -> bool

(** Helper method to attach a MediaStream to a html video element
    Arguments:
    element id - id of a Dom element to attach a stream to
    stream     - MediaStream object *)
val attachMediaStream : #Dom_html.element Js.t -> media_stream Js.t -> unit
