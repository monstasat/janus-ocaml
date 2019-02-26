open Js_of_ocaml
open Media_stream

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

module Session = Session

let attach_media_stream (elt : #Dom_html.mediaElement Js.t)
      (stream : mediaStream Js.t) : unit =
  let elt = (elt :> Dom_html.mediaElement Js.t) in
  match Adapter.get_browser () with
  | "chrome" ->
     let src_type = Js.to_string @@ Js.typeof elt##.src in
     if Adapter.get_version () >= 52
     then (Js.Unsafe.coerce elt)##.srcObject := stream
     else if (not @@ String.equal "undefined" src_type)
     then (
       let url = Dom_html.window##._URL in
       elt##.src := url##createObjectURL (Js.Unsafe.coerce stream))
     else Logs.ign_error "Error attaching stream to element"
  | _ -> (Js.Unsafe.coerce elt)##.srcObject := stream

let reattach_media_stream ~(from : #Dom_html.mediaElement Js.t)
      (_to : #Dom_html.mediaElement Js.t) : unit =
  let from = (from :> Dom_html.mediaElement Js.t) in
  let _to = (_to :> Dom_html.mediaElement Js.t) in
  let swap_obj ~from _to =
    let from_obj = (Js.Unsafe.coerce from)##.scrObject in
    (Js.Unsafe.coerce _to)##.scrObject := from_obj in
  match Adapter.get_browser () with
  | "chrome" ->
     let src_type = Js.to_string @@ Js.typeof _to##.src in
     if Adapter.get_version () >= 52
     then swap_obj ~from _to
     else if (not @@ String.equal "undefined" src_type)
     then _to##.src := from##.src
     else Logs.ign_error "Error reattaching stream to element"
  | _ -> swap_obj ~from _to

include Instance
