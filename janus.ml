type media_stream

type js_obj = (string * Js.Unsafe.any) array

class type plugin =
  object
    method getId            : unit      -> Js.number Js.t Js.meth
    method getPlugin        : unit      -> Js.js_string Js.t Js.meth
    method send             : js_obj    -> unit Js.meth
    method createAnswer     : js_obj    -> unit Js.meth
    method createOffer      : js_obj    -> unit Js.meth
    method handleRemoteJsep : js_obj    -> unit Js.meth
    method dtmf             : js_obj    -> unit Js.meth
    method data             : js_obj    -> unit Js.meth
    method getBitrate       : unit      -> Js.js_string Js.t Js.meth
    method hangup           : bool Js.t -> unit Js.meth
    method detach           : js_obj    -> unit Js.meth
  end

class type janus =
  object
    method getServer    : unit   -> Js.js_string Js.t Js.meth
    method isConnected  : unit   -> bool Js.t Js.meth
    method getSessionId : unit   -> Js.number Js.t Js.meth
    method attach       : js_obj -> unit Js.meth
    method destroy      : js_obj -> unit Js.meth
  end


let init data = Js.Unsafe.(fun_call (js_expr "Janus.init") [| obj data |]) |> ignore

let create data = Js.Unsafe.(new_obj global##._Janus [| obj data |])

let isWebrtcSupported () = Js.Unsafe.(fun_call (js_expr "Janus.isWebrtcSupported")) [||]

let attachMediaStream el_id stream =
  let el = Dom_html.document##getElementById (Js.string el_id) in
  Js.Unsafe.(fun_call (js_expr "Janus.attachMediaStream") [| inject el; inject stream |])

