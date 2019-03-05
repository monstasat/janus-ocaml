open Js_of_ocaml
open Webrtc
open Utils
open Plugin_types

let init (t : t) (dc : _RTCDataChannel Js.t) : unit =
  let on_message = fun e ->
    Log.ign_info ~inspect:e##.data "Received message on data channel:";
    Option.iter (fun f -> f @@ Js.to_string e##.data) t.on_data;
    Js._true in
  let on_state_change = fun _ ->
    let state = match t.webrtc.data_channel with
      | None -> "null"
      | Some dc -> Js.to_string dc##.readyState in
    (* FIXME change callback to on_data_state_change *)
    if String.equal state "open"
    then Option.iter (fun f -> f ()) t.on_data_open;
    Js._true in
  let on_error = fun e ->
    Log.ign_error ~inspect:e "Got error on data channel:";
    Option.iter (fun f -> f @@ Js.Unsafe.coerce e) t.on_data_error;
    Js._true in
  (* FIXME add event handlers to type *)
  let dc = Js.Unsafe.coerce dc in
  dc##.onmessage := Dom.handler on_message;
  dc##.onopen := Dom.handler on_state_change;
  dc##.onclose := Dom.handler on_state_change;
  dc##.onerror := Dom.handler on_error

let create ~(media : Media.t)
      (pc : _RTCPeerConnection Js.t) : _RTCDataChannel Js.t =
  Log.ign_info "Creating data channel";
  let options = match media.data with
    | None | Some Bool _ ->
       (* Create default options *)
       let (opts : _RTCDataChannelInit Js.t) = Js.Unsafe.obj [||] in
       opts##.ordered := Js._false;
       opts
    | Some Options { ordered
                   ; max_packet_life_time = lt
                   ; max_retransmits
                   ; protocol
                   ; negotiated
                   ; id } ->
       let (opts : _RTCDataChannelInit Js.t) = Js.Unsafe.obj [||] in
       Option.(
         iter (fun x -> opts##.ordered := Js.bool x) ordered;
         iter (fun x -> opts##.maxPacketLifeTime := Js.some x) lt;
         iter (fun x -> opts##.maxRetransmits := Js.some x) max_retransmits;
         iter (fun x -> opts##.protocol := Js.string x) protocol;
         iter (fun x -> opts##.negotiated := Js.bool x) negotiated;
         iter (fun x -> opts##.id := x) id);
       opts in
  pc##createDataChannel (Js.string "JanusDataChannel") options
