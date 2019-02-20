open Js_of_ocaml

type media_stream

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

class type janus =
  object
    method getServer : unit -> Js.js_string Js.t Js.meth
    method isConnected : unit -> bool Js.t Js.meth
    method getSessionId : unit -> Js.number Js.t Js.meth
    method attach : Jsobj.t_js -> unit Js.meth
    method destroy : Jsobj.t_js -> unit Js.meth
  end

let init (data : Jsobj.t) =
  Js.Unsafe.(fun_call (js_expr "Janus.init") [|obj data|])

let create (data : Jsobj.t) =
  Js.Unsafe.(new_obj global##._Janus [|obj data|])

let isWebrtcSupported () : bool =
  Js.to_bool @@ Js.Unsafe.(fun_call (js_expr "Janus.isWebrtcSupported") [||])

let attachMediaStream (element : #Dom_html.element Js.t) stream =
  Js.Unsafe.(
    fun_call (js_expr "Janus.attachMediaStream")
      [| inject element
       ; inject stream
      |])
