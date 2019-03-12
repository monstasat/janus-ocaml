open Js_of_ocaml
open Webrtc
open Plugin_types
open Utils

let init (t : t) (dc : _RTCDataChannel Js.t) : unit =
  let on_message = fun (e : _RTCDataChannel messageEvent Js.t) ->
    Log.ign_info ~inspect:e##.data "Received message on data channel:";
    Option.iter (fun f -> f e##.data) t.on_data;
    Js._true in
  let on_state_change = fun _ ->
    let state = match t.webrtc.data_channel with
      | None -> "null"
      | Some dc -> Js.to_string dc##.readyState in
    begin match state with
    | "open" -> Option.iter (fun f -> f ()) t.on_data_open
    | "closed" -> Option.iter (fun f -> f ()) t.on_data_close
    | _ -> ()
    end;
    Js._true in
  let on_error = fun (e : _RTCDataChannel _RTCErrorEvent Js.t) ->
    Log.ign_error ~inspect:e "Got error on data channel:";
    Option.iter (fun f -> f @@ Js.Unsafe.coerce e) t.on_data_error;
    Js._true in
  dc##.onmessage := Dom.handler on_message;
  dc##.onopen := Dom.handler on_state_change;
  dc##.onclose := Dom.handler on_state_change;
  dc##.onerror := Dom.handler on_error

let make_default_init () : _RTCDataChannelInit Js.t =
  let (opts : _RTCDataChannelInit Js.t) = Js.Unsafe.obj [||] in
  opts##.ordered := Js._false;
  opts

let create (init : _RTCDataChannelInit Js.t)
      (pc : _RTCPeerConnection Js.t) : _RTCDataChannel Js.t =
  Log.ign_info "Creating data channel";
  pc##createDataChannel (Js.string "JanusDataChannel") init
