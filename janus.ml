open Js_of_ocaml
open Webrtc
open Utils

module Webrtc = Webrtc
module Plugin = Plugin
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
     else Log.ign_error "Error attaching stream to element"
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
     else Log.ign_error "Error reattaching stream to element"
  | _ -> swap_obj ~from _to


type t =
  { mutable close_listener : Dom_events.listener option
  ; mutable sessions : (int * Session.t) list
  }

let is_webrtc_supported () : bool =
  Utils.is_webrtc_supported ()

let detect_tab_close (t : t) : Dom_events.listener =
  let (ios : bool) =
    Js.to_string @@ Dom_html.window##.navigator##.platform
    |> fun x -> List.mem ~eq:String.equal x ["iPad"; "iPhone"; "iPod"] in
  let (event : string) = if ios then "pagehide" else "beforeunload" in
  let old_bf =
    Js.Unsafe.get Dom_html.window (Js.string @@ "on" ^ event)
    |> fun x ->
       if String.equal (Js.to_string @@ Js.typeof x) "function"
       then Some x else None in
  Dom_events.listen Dom_html.window (Dom_events.Typ.make event) (fun _ _ ->
      Log.ign_info "Closing window";
      List.iter (fun (id, (s : Session.t)) ->
          if Session.destroy_on_unload s
          then (
            Log.ign_info_f "Destroying session: %d" id;
            Session.destroy ~async_request:false ~notify_destroyed:false s
            |> Lwt.ignore_result))
        t.sessions;
      Option.iter (fun f -> Js.Unsafe.fun_call f [||]) old_bf;
      true)

let create_session (t : t) (props : Session.properties)
    : (Session.t, string) Lwt_result.t =
  let on_create = fun (s : Session.t) ->
    let id = Session.id s in
    t.sessions <- List.set_assoc ~eq:(=) id s t.sessions in
  let on_destroy = fun (s : Session.t) ->
    let id = Session.id s in
    t.sessions <- List.remove_assoc id t.sessions in
  Session.create_session ~on_create ~on_destroy ~props ()

let create ?(log_level = Lwt_log_js.Error) () : t =
  Log.Section.set_level Log.section log_level;
  Log.ign_info "Initializing library";
  let t =
    { sessions = []
    ; close_listener = None
    } in
  { t with close_listener = Some (detect_tab_close t) }
