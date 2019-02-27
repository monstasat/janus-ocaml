open Js_of_ocaml
open Webrtc
open Utils
open Lwt_result.Infix

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
  { close_listener : Dom_events.listener
  ; sessions : (int * Session.t) list ref
  }

let is_webrtc_supported () : bool =
  Utils.is_webrtc_supported ()

let detect_tab_close (sessions : (int * Session.t) list ref)
    : Dom_events.listener =
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
      Log.ign_info_f "Closing window";
      List.iter (fun (_, (s : Session.t)) ->
          if Session.destroy_on_unload s
          then (
            Log.ign_info_f "Destroying session: %d" @@ Session.id s;
            Session.destroy ~async_request:false ~notify_destroyed:false s
            |> Lwt.ignore_result))
        !sessions;
      Option.iter (fun f -> Js.Unsafe.fun_call f [||]) old_bf;
      true)

let sessions ({ sessions; _ } : t) : Session.t list =
  List.map snd !sessions

let rec create_session ?remaining
          ?(ice_servers : _RTCIceServer Js.t list option)
          ?(ipv6 = false)
          ?(with_credentials = false)
          ?(max_poll_events = 10)
          ?(token : string option)
          ?(apisecret : string option)
          ?ice_transport_policy
          ?bundle_policy
          ?(destroy_on_unload = true)
          ?(keep_alive_period = 25000)
          ?(long_poll_timeout = 60000)
          ?(on_destroyed : (unit -> unit) option)
          ?(on_error : (string -> unit) option)
          ?(aux_servers : string list option)
          ~(server : string)
          (t : t)
        : (Session.t, string) Lwt_result.t =
  is_webrtc_supported_lwt ()
  >>= fun () ->
  (* TODO websockets *)
  let message =
    Api.Msg.make_req
      ?token
      ?apisecret
      ~janus:"create"
      ~transaction:(String.random 12)
      () in
  (* TODO websockets *)
  let server, remaining = match remaining with
    (* Connecting at first time *)
    | None -> server, aux_servers
    (* Trying remaining servers *)
    | Some (hd, tl) -> hd, Some tl in
  Lwt.Infix.(
    Api.http_call
      ~with_credentials
      ~body:message
      ~meth:`POST
      server
    >>= function
    | Ok (rsp : Api.Msg.msg Js.t) ->
       (match Api.Msg.check_msg rsp with
        | Error e -> Lwt_result.fail e
        | Ok rsp ->
           let (rsp : Api.Msg.rsp_id Js.t) = Js.Unsafe.coerce rsp in
           let id = rsp##.data##.id in
           Log.ign_debug_f "Created session: %d" id;
           let ice_servers = match ice_servers with
             | None | Some [] -> default_ice ()
             | Some l -> l in
           let cleanup = fun (id : int) ->
             t.sessions := List.remove_assoc id !(t.sessions) in
           let plugins = ref [] in
           let max_poll_events = max 1 max_poll_events in
           let event_loop =
             Session.event_handler ?token ?apisecret
               ~id ~server ~plugins ~long_poll_timeout ~max_poll_events
               ~with_credentials () in
           (* Notify client if event loop fails *)
           notify_error_lwt event_loop on_error;
           let (s : Session.t) =
             { id
             ; server
             ; plugins
             ; ice_servers
             ; ice_transport_policy
             ; bundle_policy
             ; ipv6
             ; with_credentials
             ; max_poll_events
             ; token
             ; apisecret
             ; destroy_on_unload
             ; keep_alive_period
             ; long_poll_timeout
             ; on_destroyed
             ; on_error
             ; cleanup
             ; event_loop = Some event_loop
             } in
           t.sessions := List.set_assoc ~eq:(=) id s !(t.sessions);
           Lwt_result.return s)
    | Error e ->
       (match remaining with
        | Some [] ->
           "Error connecting to any of the provided Janus servers: \
            Is the server down?"
           |> Lwt_result.fail
        | Some (hd :: tl) ->
           (* Let's try the next server *)
           Lwt_js.sleep 200.
           >>= (fun () ->
            create_session ?ice_servers ~ipv6
              ~with_credentials ~max_poll_events
              ?token ?apisecret ?ice_transport_policy
              ?bundle_policy ~destroy_on_unload ~keep_alive_period
              ~long_poll_timeout ?on_destroyed
              ~server ~remaining:(hd, tl) t)
        | None -> Lwt_result.fail (Api.error_to_string e)))

let create ?(log_level = Lwt_log_js.Error) () : t =
  Log.Section.set_level Log.section log_level;
  Log.ign_info "Initializing library";
  let sessions = ref [] in
  { sessions
  ; close_listener = detect_tab_close sessions
  }
