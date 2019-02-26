open Js_of_ocaml
open Utils
open Lwt.Infix
open Webrtc

let empty_response = "The response is empty"

type promises =
  { destroy : unit Lwt.t
  ; error : string Lwt.t
  }

type reconnect =
  { server : string
  ; id : int
  }

type plugin_props =
  { ice_servers : _RTCIceServer Js.t list
  }

type properties =
  { server : string * string list
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
  }

type t =
  { id : int
  ; server : string
  ; mutable connected : bool
  ; mutable plugins : (int * Plugin.t) list
  ; props : properties
  ; on_create : t -> unit
  ; on_destroy : t -> unit
  }

let make ?(connected = true) ?(plugins = [])
      ~on_create ~on_destroy
      ~id ~server ~props () : t =
  { id
  ; server
  ; connected
  ; plugins
  ; props
  ; on_create
  ; on_destroy
  }

let make_properties
      ?(ice_servers : _RTCIceServer Js.t list option)
      ?(ipv6 = false)
      ?(with_credentials = false)
      ?(max_poll_events = 10)
      ?token
      ?apisecret
      ?ice_transport_policy
      ?bundle_policy
      ?(destroy_on_unload = true)
      ?(keep_alive_period = 25000)
      ?(long_poll_timeout = 60000)
      ~server
      () : properties =
  let ice_servers = match ice_servers with
    | None | Some [] ->
       let url = "stun:stun.l.google.com:19302" in
       let (o : _RTCIceServer Js.t) = Js.Unsafe.obj [||] in
       o##.urls := Js.string url;
       [o]
    | Some l -> l in
  let max_poll_events = if max_poll_events < 1 then 1 else max_poll_events in
  { server
  ; ice_servers
  ; ipv6
  ; with_credentials
  ; max_poll_events
  ; token
  ; apisecret
  ; destroy_on_unload
  ; keep_alive_period
  ; long_poll_timeout
  ; ice_transport_policy
  ; bundle_policy
  }

let do_when_sender ?(warn = true) (t : t) (sender : int Js.optdef)
    (f : Plugin.t -> unit) : unit =
  match Js.Optdef.to_option sender with
  | None -> Log.ign_warning "Missing sender"
  | Some id ->
     match List.assoc_opt id t.plugins with
     | None ->
        if warn
        then Log.ign_debug "This handle is not attached to this session"
     | Some (p : Plugin.t) -> f p

let handle_event (t : t) (event : Api.Msg.msg Js.t) : unit =
  (match Js.to_string event##.janus with
   | "keepalive" ->
      (* Nothing happened *)
      Log.ign_debug_f "Got a keepalive on session %d" t.id
   | "ack" ->
      (* Just an ack, we can probably ignore *)
      Log.ign_debug_f "Got an ack on session %d" t.id;
      Log.ign_debug ~inspect:event "";
      begin match Js.Optdef.to_option event##.transaction with
      | None -> ()
      | Some _ -> () (* TODO report success when websockets *)
      end
   | "success" ->
      (* Success! *)
      Log.ign_debug_f "Got a success on session %d" t.id;
      Log.ign_debug ~inspect:event "";
      begin match Js.Optdef.to_option event##.transaction with
      | None -> ()
      | Some _ -> () (* TODO report success when websockets *)
      end
   | "trickle" -> ()
   | "webrtcup" ->
      (* The PeerConnection with the server is up! Notify this *)
      Log.ign_debug_f "Got a webrtcup on session %d" t.id;
      Log.ign_debug ~inspect:event "";
      let (event : Api.Msg.webrtc_event_base Js.t) =
        Js.Unsafe.coerce event in
      do_when_sender t event##.sender (fun (p : Plugin.t) ->
          match p.callbacks.webrtc_state with
          | None -> ()
          | Some f -> f Up)
   | "hangup" ->
      (* A plugin asked the core to hangup a PeerConnection
         on one of our handles *)
      Log.ign_debug_f "Got a hangup event on session %d" t.id;
      Log.ign_debug ~inspect:event "";
      let (event : Api.Msg.webrtc_event_hangup Js.t) =
        Js.Unsafe.coerce event in
      do_when_sender t event##.sender (fun (p : Plugin.t) ->
          (match p.callbacks.webrtc_state with
           | None -> ()
           | Some f -> f (Down (Js.to_string event##.reason)));
          Plugin.hangup p)
   | "detached" ->
      (* A plugin asked the core to detach one of our handles *)
      Log.ign_debug_f "Got a detached event on session %d" t.id;
      Log.ign_debug ~inspect:event "";
      let (event : Api.Msg.webrtc_event_base Js.t) =
        Js.Unsafe.coerce event in
      do_when_sender ~warn:false t event##.sender (fun (p : Plugin.t) ->
          p.detached <- true;
          (match p.callbacks.on_detached with
           | None -> ()
           | Some f -> f ());
          Plugin.detach p)
   | "media" ->
      (* Media started/stopped flowing *)
      Log.ign_debug_f "Got a media event on session %d" t.id;
      Log.ign_debug ~inspect:event "";
      let (event : Api.Msg.webrtc_event_media Js.t) =
        Js.Unsafe.coerce event in
      do_when_sender t event##.sender (fun (p : Plugin.t) ->
          match p.callbacks.media_state with
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
      Log.ign_debug_f "Got a slowlink event on session %d" t.id;
      Log.ign_debug ~inspect:event "";
      let (event : Api.Msg.webrtc_event_slowlink Js.t) =
        Js.Unsafe.coerce event in
      do_when_sender t event##.sender (fun (p : Plugin.t) ->
          match p.callbacks.slow_link with
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
      Log.ign_debug_f "Got a plugin event on session %d" t.id;
      Log.ign_debug ~inspect:event "";
      let (event : Api.Msg.handle_event Js.t) =
        Js.Unsafe.coerce event in
      do_when_sender t event##.sender (fun (p : Plugin.t) ->
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
             match p.callbacks.on_message with
             | None -> Log.ign_debug "No provided notification callback"
             | Some f ->
                Log.ign_debug "Notifying application";
                f ?jsep data)
   | "timeout" ->
      (* FIXME close websockets *)
      Log.ign_error_f "Timeout on session %d" t.id;
      Log.ign_debug ~inspect:event ""
   | janus ->
      Log.ign_warning_f "Unknown message/event '%s' on session %d" janus t.id;
      Log.ign_debug ~inspect:event "")

let rec event_handler ?(retries = 0) (t : t) : unit Lwt.t =
  Log.ign_debug "Long poll...";
  if not t.connected
  then (
    Log.ign_warning "Is the server down? (connected=false)";
    Lwt.return_unit)
  else
    let opt_string_to_query (name : string) = function
      | None -> None
      | Some s ->
         let v = Js.to_string @@ Js.encodeURIComponent (Js.string s) in
         Some (name, [v]) in
    let now = Int64.of_float @@ new%js Js.date_now##getTime in
    let query =
      [ "rid", [Printf.sprintf "%Ld" now]
      ; "maxev", [string_of_int t.props.max_poll_events]
      ]
      |> List.cons_maybe (opt_string_to_query "token" t.props.token)
      |> List.cons_maybe (opt_string_to_query "apisecret" t.props.apisecret) in
    Api.http_call ~meth:`GET
      ~timeout:t.props.long_poll_timeout
      ~with_credentials:t.props.with_credentials
      (add_query (Printf.sprintf "%s/%d" t.server t.id) query)
    >>= function
    | Ok x ->
       (match cast_list x with
       | None -> handle_event t x
       | Some l -> List.iter (handle_event t) l);
       event_handler t
    | Error e ->
       Log.ign_error @@ Api.error_to_string e;
       if retries > 2
       then (
         t.connected <- false;
         Lwt.return_unit)
       else event_handler ~retries:(succ retries) t

let rec create_session
          ~(props : properties)
          ~on_create
          ~on_destroy
          ?remaining
          ?(reconnect : reconnect option)
          ()
        : (t, string) Lwt_result.t =
  if not @@ is_webrtc_supported ()
  then Lwt.return_error "WebRTC is not supported by this browser"
  else (
    (* TODO websockets *)
    let message =
      Api.Msg.make_req
        ?session_id:(Option.map (fun (x : reconnect) -> x.id) reconnect)
        ~janus:(if Option.is_some reconnect then "claim" else "create")
        ~transaction:(String.random 12)
        ?token:props.token
        ?apisecret:props.apisecret
        () in
    (* TODO websockets *)
    let server, remaining = match reconnect, remaining with
      (* Reconnecting the session *)
      | Some { server; _ }, _ -> server, None
      (* Connecting at first time *)
      | None, None -> let s, r = props.server in s, Some r
      (* Trying remaining servers *)
      | None, Some (hd, tl) -> hd, Some tl in
    Api.http_call
      ~with_credentials:props.with_credentials
      ~body:message
      ~meth:`POST
      server
    >>= function
    | Ok (rsp : Api.Msg.msg Js.t) ->
       (match Api.Msg.check_err rsp with
        | Error e -> Lwt_result.fail e
        | Ok rsp ->
           let (rsp : Api.Msg.rsp_id Js.t) = Js.Unsafe.coerce rsp in
           let id = rsp##.data##.id in
           let (t : t) = make ~id ~server ~props ~on_create ~on_destroy () in
           if Option.is_some reconnect
           then Log.ign_debug_f "Claimed session: %d" id
           else Log.ign_debug_f "Created session: %d" id;
           on_create t;
           Lwt.ignore_result @@ event_handler t;
           Lwt_result.return t)
    | Error e ->
       (match snd props.server, remaining with
        | (_ :: _), Some [] ->
           "Error connecting to any of the provided Janus servers: \
            Is the server down?"
           |> Lwt_result.fail
        | (_ :: _), Some (hd :: tl) ->
           (* Let's try the next server *)
           Lwt_js.sleep 200.
           >>= (create_session ~on_create ~on_destroy
                  ~remaining:(hd, tl) ~props)
        | [], _ | _, None -> Lwt_result.fail (Api.error_to_string e)))

let id (t : t) : int =
  t.id

let server (t : t) : string =
  t.server

let servers (t : t) : string list =
  let a, b = t.props.server in
  a :: b

let ice_servers (t : t) : _RTCIceServer Js.t list =
  t.props.ice_servers

let ipv6 (t : t) : bool =
  t.props.ipv6

let with_credentials (t : t) : bool =
  t.props.with_credentials

let max_poll_events (t : t) : int =
  t.props.max_poll_events

let token (t : t) : string option =
  t.props.token

let apisecret (t : t) : string option =
  t.props.apisecret

let destroy_on_unload (t : t) : bool =
  t.props.destroy_on_unload

let keep_alive_period (t : t) : int =
  t.props.keep_alive_period

let long_poll_timeout (t : t) : int =
  t.props.long_poll_timeout

let connected (t : t) : bool =
  t.connected

let attach_plugin ?(opaque_id : string option)
      ?(token : string option)
      ~(plugin : string)
      (t : t) : (Plugin.t, string) Lwt_result.t =
  ignore (opaque_id, token);
  if not t.connected
  then (
    let s = "Is the server down? (connected=false)" in
    Log.ign_error s;
    Lwt.return_error s)
  else (
    let (message : Api.Msg.req Js.t) =
      Api.Msg.make_req
        ?opaque_id
        ?token:t.props.token
        ?apisecret:t.props.apisecret
        ~plugin
        ~transaction:(String.random 12)
        ~janus:"attach"
        () in
    (* TODO websosckets *)
    Api.http_call ~meth:`POST
      ~with_credentials:t.props.with_credentials
      ~body:message
      (Printf.sprintf "%s/%d" t.server t.id)
    >|= function
    | Error e -> Error (Api.error_to_string e)
    | Ok rsp ->
       match Api.Msg.check_err rsp with
       | Error e -> Error e
       | Ok rsp ->
          let (rsp : Api.Msg.rsp_id Js.t) = Js.Unsafe.coerce rsp in
          let id = rsp##.data##.id in
          Log.ign_info_f "Created plugin: %d" id;
          (* FIXME *)
          Ok (Obj.magic ()))

let destroy ?(async_request = true)
      ?(notify_destroyed = true) (t : t)
    : (unit, string) Lwt_result.t =
  Log.ign_info_f "destroying session %d (async=%b)" t.id async_request;
  if not t.connected
  then (
    Log.ign_warning "Is the server down? (connected=false)";
    Lwt.return_ok ())
  else (
    let (message : Api.Msg.req Js.t) =
      Api.Msg.make_req
        ~janus:"destroy"
        ~transaction:(String.random 12)
        ?apisecret:t.props.apisecret
        ?token:t.props.token
        () in
    Api.http_call ~meth:`POST
      ~async:async_request
      ~with_credentials:t.props.with_credentials
      ~body:message
      (Printf.sprintf "%s/%d" t.server t.id)
    >|= function
    | Error e -> Error (Api.error_to_string e)
    | Ok rsp ->
       match Api.Msg.check_err rsp with
       | Error e -> Error e
       | Ok _ ->
          t.connected <- false;
          t.on_destroy t;
          if notify_destroyed then () (* FIXME *);
          Ok ())

let reconnect ?ice_servers ?ipv6 ?with_credentials ?max_poll_events
      ?token ?apisecret ?destroy_on_unload
      ?keep_alive_period ?long_poll_timeout ~server
      (t : t)
    : (t, string) Lwt_result.t =
  let props =
    make_properties ?ice_servers ?ipv6 ?with_credentials ?max_poll_events
      ?token ?apisecret ?destroy_on_unload
      ?keep_alive_period ?long_poll_timeout ~server () in
  t.connected <- false;
  create_session
    ~on_create:t.on_create
    ~on_destroy:t.on_destroy
    ~reconnect:{ id = t.id; server = t.server }
    ~props
    ()
