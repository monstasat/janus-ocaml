open Js_of_ocaml
open Utils
open Lwt_result.Infix
open Webrtc

type t =
  { id : int
  ; server : string
  ; plugins : (int * Plugin.t) list ref
  ; mutable event_loop : string Lwt.t option
  (* Properties *)
  ; ice_servers : _RTCIceServer Js.t list
  ; ice_transport_policy : Plugin.ice_transport_policy option
  ; bundle_policy : Plugin.bundle_policy option
  ; ipv6 : bool
  ; with_credentials : bool
  ; max_poll_events : int
  ; token : string option
  ; apisecret : string option
  ; destroy_on_unload : bool
  ; keep_alive_period : int
  ; long_poll_timeout : int
  (* Callbacks *)
  ; on_error : (string -> unit) option
  ; on_destroyed : (unit -> unit) option
  ; cleanup : (int -> unit)
  }

let do_when_sender ?(warn = true)
      (plugins : (int * Plugin.t) list ref)
      (sender : int Js.optdef)
      (f : Plugin.t -> unit) : unit =
  match Js.Optdef.to_option sender with
  | None -> Log.ign_warning "Missing sender"
  | Some id ->
     match List.assoc_opt id !plugins with
     | None ->
        if warn
        then Log.ign_debug_f "Handle %d is not attached to this session" id
     | Some (p : Plugin.t) -> f p

let handle_event ~(id : int)
      ~(plugins : (int * Plugin.t) list ref)
      (event : Api.Msg.msg Js.t) : unit =
  (match Js.to_string event##.janus with
   | "keepalive" ->
      (* Nothing happened *)
      Log.ign_debug_f "Got a keepalive on session %d" id
   | "ack" ->
      (* Just an ack, we can probably ignore *)
      Log.ign_debug_f "Got an ack on session %d" id;
      Log.ign_debug ~inspect:event "";
      begin match Js.Optdef.to_option event##.transaction with
      | None -> ()
      | Some _ -> () (* TODO report success when websockets *)
      end
   | "success" ->
      (* Success! *)
      Log.ign_debug_f "Got a success on session %d" id;
      Log.ign_debug ~inspect:event "";
      begin match Js.Optdef.to_option event##.transaction with
      | None -> ()
      | Some _ -> () (* TODO report success when websockets *)
      end
   | "trickle" ->
      (* We got a trickle candidate from Janus *)
      let (event : Api.Msg.webrtc_event_base Js.t) =
        Js.Unsafe.coerce event in
      do_when_sender plugins event##.sender (fun (p : Plugin.t) ->
          let (candidate : _RTCIceCandidateInit Js.t) =
            (Js.Unsafe.coerce event)##.candidate in
          Log.ign_debug_f "Got a trickled candidate on session %d" id;
          Log.ign_debug ~inspect:candidate "";
          match p.webrtc.pc, p.webrtc.remote_sdp with
          | None, _ | _, None ->
             (* We didn't do setRemoteDescription
                (trickle got here before the offer?) *)
             Log.ign_debug "We didn't do setRemoteDescription \
                            (trickle got here before the offer?), \
                            caching candidate";
             ignore @@ p.webrtc.candidates##push candidate;
             Log.ign_debug ~inspect:p.webrtc.candidates ""
          | Some pc, Some _ ->
             Log.ign_debug ~inspect:candidate "Adding remote candidate:";
             let (completed : bool) =
               (Js.Unsafe.coerce candidate)##.competed
               |> (fun x -> Js.Optdef.get x (fun () -> Js._false))
               |> Js.to_bool in
             if completed
             then ignore @@ pc##addIceCandidate (Js.Unsafe.obj [||])
             else ignore @@ pc##addIceCandidate candidate;
        )
   | "webrtcup" ->
      (* The PeerConnection with the server is up! Notify this *)
      Log.ign_debug_f "Got a webrtcup on session %d" id;
      Log.ign_debug ~inspect:event "";
      let (event : Api.Msg.webrtc_event_base Js.t) =
        Js.Unsafe.coerce event in
      do_when_sender plugins event##.sender (fun (p : Plugin.t) ->
          match p.on_webrtc_state with
          | None -> ()
          | Some f -> f Up)
   | "hangup" ->
      (* A plugin asked the core to hangup a PeerConnection
         on one of our handles *)
      Log.ign_debug_f "Got a hangup event on session %d" id;
      Log.ign_debug ~inspect:event "";
      let (event : Api.Msg.webrtc_event_hangup Js.t) =
        Js.Unsafe.coerce event in
      do_when_sender plugins event##.sender (fun (p : Plugin.t) ->
          (match p.on_webrtc_state with
           | None -> ()
           | Some f -> f (Down (Js.to_string event##.reason)));
          Plugin.hangup p)
   | "detached" ->
      (* A plugin asked the core to detach one of our handles *)
      Log.ign_debug_f "Got a detached event on session %d" id;
      Log.ign_debug ~inspect:event "";
      let (event : Api.Msg.webrtc_event_base Js.t) =
        Js.Unsafe.coerce event in
      do_when_sender ~warn:false plugins event##.sender (fun (p : Plugin.t) ->
          p.detached <- true;
          Option.iter (fun f -> f ()) p.on_detached;
          Lwt.ignore_result @@ Plugin.detach p)
   | "media" ->
      (* Media started/stopped flowing *)
      Log.ign_debug_f "Got a media event on session %d" id;
      Log.ign_debug ~inspect:event "";
      let (event : Api.Msg.webrtc_event_media Js.t) =
        Js.Unsafe.coerce event in
      do_when_sender plugins event##.sender (fun (p : Plugin.t) ->
          match p.on_media_state with
          | None -> ()
          | Some f ->
             let typ = event##._type in
             let receiving = Js.to_bool event##.receiving in
             match Js.to_string typ with
             | "audio" -> f (Audio receiving)
             | "video" -> f (Video receiving)
             | s -> Log.ign_warning_f "Got unknown media type: %s" s)
   | "slowlink" ->
      (* Trouble uplink or downlink *)
      Log.ign_debug_f "Got a slowlink event on session %d" id;
      Log.ign_debug ~inspect:event "";
      let (event : Api.Msg.webrtc_event_slowlink Js.t) =
        Js.Unsafe.coerce event in
      do_when_sender plugins event##.sender (fun (p : Plugin.t) ->
          match p.on_slow_link with
          | None -> ()
          | Some f -> f { nacks = event##.nacks
                        ; uplink = Js.to_bool event##.uplink })
   | "error" ->
      (* Oops, something wrong happened *)
      begin match Js.Optdef.to_option event##.error with
      | None -> Log.ign_error "Oops: internal error"
      | Some (error : Api.Msg.err Js.t) ->
         let code = error##.code in
         let reason = Js.to_string error##.reason in
         Log.ign_error_f "Oops: %d %s" code reason;
      end;
      Log.ign_debug ~inspect:event "";
      begin match Js.Optdef.to_option event##.transaction with
      | None -> ()
      | Some _ -> () (* TODO report success when websockets *)
      end
   | "event" ->
      Log.ign_debug_f "Got a plugin event on session %d" id;
      Log.ign_debug ~inspect:event "";
      let (event : Api.Msg.handle_event Js.t) =
        Js.Unsafe.coerce event in
      do_when_sender plugins event##.sender (fun (p : Plugin.t) ->
          match Js.Optdef.to_option event##.plugindata with
          | None -> Log.ign_warning "Missing plugindata..."
          | Some (plugindata : Api.Msg.plugindata Js.t) ->
             let plugin = Js.to_string plugindata##.plugin in
             let data = plugindata##.data in
             let jsep = Js.Optdef.to_option event##.jsep in
             let id = Plugin.id p in
             Log.ign_debug_f " -- Event is coming from %d (%s)" id plugin;
             Log.ign_debug ~inspect:data "";
             begin match jsep with
             | None -> ()
             | Some js ->
                Log.ign_debug "Handling SDP as well";
                Log.ign_debug ~inspect:js ""
             end;
             match p.on_message with
             | None -> Log.ign_debug "No provided notification callback"
             | Some f ->
                Log.ign_debug "Notifying application";
                f ?jsep data p)
   | "timeout" ->
      (* FIXME close websockets *)
      Log.ign_error_f "Timeout on session %d" id;
      Log.ign_debug ~inspect:event ""
   | janus ->
      Log.ign_warning_f "Unknown message/event '%s' on session %d" janus id;
      Log.ign_debug ~inspect:event "")

let event_handler ?(token : string option)
      ?(apisecret : string option)
      ~(id : int)
      ~(server : string)
      ~(plugins : (int * Plugin.t) list ref)
      ~(long_poll_timeout : int)
      ~(max_poll_events : int)
      ~(with_credentials : bool)
      () : string Lwt.t =
  Log.ign_debug_f "Starting long poll event loop for session %d" id;
  let opt_string_to_query (name : string) = function
    | None -> None
    | Some s ->
       let v = Js.to_string @@ Js.encodeURIComponent (Js.string s) in
       Some (name, [v]) in
  let query =
    ["maxev", [string_of_int max_poll_events]]
    |> List.cons_maybe (opt_string_to_query "token" token)
    |> List.cons_maybe (opt_string_to_query "apisecret" apisecret) in
  let uri = add_query (Printf.sprintf "%s/%d" server id) query in
  let rec aux ?(retries = 0) () =
    Log.ign_debug "Long poll...";
    Lwt.Infix.(
      let now = Int64.of_float @@ new%js Js.date_now##getTime in
      Api.http_call ~meth:`GET
        ~timeout:long_poll_timeout
        ~with_credentials
        (match query with
         | [] -> Printf.sprintf "%s?rid=%Ld" uri now
         | _ -> Printf.sprintf "%s&rid=%Ld" uri now)
      >>= function
      | Ok x ->
         Log.ign_info "Got event";
         (match cast_list x with
          | None -> handle_event ~id ~plugins x
          | Some l -> List.iter (handle_event ~id ~plugins) l);
         aux ()
      | Error e ->
         if retries < 3 then aux ~retries:(succ retries) () else (
           Lwt.return @@ Api.error_to_string e)) in
  aux ()

let id (t : t) : int =
  t.id

let server (t : t) : string =
  t.server

let ice_servers (t : t) : _RTCIceServer Js.t list =
  t.ice_servers

let ipv6 (t : t) : bool =
  t.ipv6

let with_credentials (t : t) : bool =
  t.with_credentials

let max_poll_events (t : t) : int =
  t.max_poll_events

let token (t : t) : string option =
  t.token

let apisecret (t : t) : string option =
  t.apisecret

let destroy_on_unload (t : t) : bool =
  t.destroy_on_unload

let keep_alive_period (t : t) : int =
  t.keep_alive_period

let long_poll_timeout (t : t) : int =
  t.long_poll_timeout

let connected (t : t) : bool =
  match t.event_loop with
  | None -> false
  | Some t -> Lwt.is_sleeping t

let attach_plugin ?(opaque_id : string option)
      ?(token : string option)
      ?(rtc_constraints = [])
      ?on_local_stream
      ?on_remote_stream
      ?on_message
      ?on_consent_dialog
      ?on_ice_state
      ?on_webrtc_state
      ?on_media_state
      ?on_slow_link
      ?on_data
      ?on_data_open
      ?on_data_close
      ?on_data_error
      ?on_cleanup
      ?(on_detached : (unit -> unit) option)
      ~(typ : Plugin.typ)
      (t : t) : (Plugin.t, string) Lwt_result.t =
  is_connected_lwt (fun () -> connected t)
  >>= fun () ->
  let token = match token with
    | None -> t.token
    | Some x -> Some x in
  let (message : Api.Msg.req Js.t) =
    Api.Msg.make_req
      ?token
      ?opaque_id
      ?apisecret:t.apisecret
      ~plugin:(Plugin.typ_to_string typ)
      ~transaction:(String.random 12)
      ~janus:"attach"
      () in
  (* TODO websosckets *)
  Lwt.Infix.(
    Api.http_call ~meth:`POST
      ~with_credentials:t.with_credentials
      ~body:message
      (Printf.sprintf "%s/%d" t.server t.id)
    >|= function
    | Error e -> Error (Api.error_to_string e)
    | Ok rsp ->
       match Api.Msg.check_msg rsp with
       | Error e -> Error e
       | Ok rsp ->
          let (rsp : Api.Msg.rsp_id Js.t) = Js.Unsafe.coerce rsp in
          let id = rsp##.data##.id in
          let on_message = match on_message with
            | None -> None
            | Some f -> Some (fun ?jsep m -> f ?jsep (Js.Unsafe.coerce m)) in
          let rm_from_session = fun (id : int) ->
            t.plugins := List.remove_assoc id !(t.plugins) in
          let (p : Plugin.t) =
            { id
            ; opaque_id
            ; server = t.server
            ; session_id = t.id
            ; is_connected = (fun () -> connected t)
            ; plugin = typ
            ; token
            ; apisecret = t.apisecret
            ; ipv6 = t.ipv6
            ; rtc_constraints
            ; with_credentials = t.with_credentials
            ; ice_servers = t.ice_servers
            ; ice_transport_policy = t.ice_transport_policy
            ; bundle_policy = t.bundle_policy
            ; webrtc = Plugin.Webrtc_stuff.make_empty ()
            ; detached = false
            ; on_local_stream
            ; on_remote_stream
            ; on_message
            ; on_consent_dialog
            ; on_ice_state
            ; on_webrtc_state
            ; on_media_state
            ; on_slow_link
            ; on_data
            ; on_data_open
            ; on_data_close
            ; on_data_error
            ; on_cleanup
            ; on_detached
            ; rm_from_session
            } in
          t.plugins := List.set_assoc ~eq:(=) id p !(t.plugins);
          Log.ign_info_f "Created plugin: %d" id;
          Ok p)

let reconnect ({ id
               ; server
               ; with_credentials
               ; token
               ; apisecret
               ; plugins
               ; long_poll_timeout
               ; max_poll_events
               ; on_error
               ; _ } as t : t) : (unit, string) Lwt_result.t =
  (* Stop event loop *)
  Option.iter Lwt.cancel t.event_loop;
  t.event_loop <- None;
  (* Request reconnect *)
  let message =
    Api.Msg.make_req ?token ?apisecret
      ~janus:"claim"
      ~session_id:id
      ~transaction:(String.random 12)
      () in
  (* TODO websockets *)
  Lwt.Infix.(
    Api.http_call ~with_credentials ~body:message ~meth:`POST server
    >>= function
    | Error e -> Lwt_result.fail (Api.error_to_string e)
    | Ok (rsp : Api.Msg.msg Js.t) ->
       (match Api.Msg.check_msg rsp with
        | Error e -> Lwt_result.fail e
        | Ok (rsp : Api.Msg.msg Js.t) ->
           let id = Js.Optdef.get rsp##.session_id_ (fun () -> id) in
           Log.ign_debug_f "Claimed session: %d" id;
           let event_loop =
             event_handler ?token ?apisecret ~with_credentials
               ~id ~server ~plugins ~long_poll_timeout
               ~max_poll_events () in
           notify_error_lwt event_loop on_error;
           t.event_loop <- Some event_loop;
           Lwt_result.return ()))

let destroy ?(async = true)
      ?(notify_destroyed = true) (t : t)
    : (unit, string) Lwt_result.t =
  Log.ign_info_f "destroying session %d (async=%b)" t.id async;
  is_connected_lwt (fun () -> connected t)
  >>= fun () ->
  (* Stop event loop *)
  Option.iter Lwt.cancel t.event_loop;
  t.event_loop <- None;
  (* Request destroy *)
  let (message : Api.Msg.req Js.t) =
    Api.Msg.make_req
      ~janus:"destroy"
      ~transaction:(String.random 12)
      ?apisecret:t.apisecret
      ?token:t.token
      () in
  (* TODO add websockets *)
  Lwt.Infix.(
    Api.http_call ~meth:`POST
      ~async
      ~with_credentials:t.with_credentials
      ~body:message
      (Printf.sprintf "%s/%d" t.server t.id)
    >|= function
    | Error e -> Error (Api.error_to_string e)
    | Ok rsp ->
       match Api.Msg.check_msg rsp with
       | Error e -> Error e
       | Ok _ ->
          begin match notify_destroyed, t.on_destroyed with
          | false, _ | _, None -> ()
          | true, Some f -> f ()
          end;
          t.cleanup t.id;
          Ok ())

