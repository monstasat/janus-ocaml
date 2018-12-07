open Js_of_ocaml

type media_stream

type js_obj = (string * Js.Unsafe.any) array

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

class type janus =
  object
    method getServer : unit -> Js.js_string Js.t Js.meth
    method isConnected : unit -> bool Js.t Js.meth
    method getSessionId : unit -> Js.number Js.t Js.meth
    method attach : js_obj -> unit Js.meth
    method destroy : js_obj -> unit Js.meth
  end

open Js.Unsafe

let init data =
  fun_call (js_expr "Janus.init") [| obj data |]
  |> ignore

let create data =
  new_obj global##._Janus [| obj data |]

let isWebrtcSupported ()
  = fun_call (js_expr "Janus.isWebrtcSupported") [||]

let attachMediaStream (element:#Dom_html.element Js.t) stream =
  fun_call (js_expr "Janus.attachMediaStream")
    [| inject element
     ; inject stream
    |]
