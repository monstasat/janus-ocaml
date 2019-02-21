open Js_of_ocaml
open Utils
open Lwt.Infix
open Types

let empty_response = "The response is empty"

type properties =
  { server : Uri.t * Uri.t list
  ; ice_servers : Rtc_peer_connection.ICE.t list
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
  { id : int64
  ; server : Uri.t
  ; mutable connected : bool
  ; mutable plugins : Plugin.t list
  ; props : properties
  ; on_create : t -> unit
  ; on_destroy : t -> unit
  ; logs : logs
  }

let make ?(connected = true) ?(plugins = [])
      ~on_create ~on_destroy
      ~logs ~id ~server ~props () : t =
  { id; server; connected; plugins; props; logs; on_create; on_destroy }

let make_properties
      ?(ice_servers = Rtc_peer_connection.ICE.default)
      ?(ipv6 = false)
      ?(with_credentials = false)
      ?(max_poll_events = 10)
      ?token
      ?apisecret
      ?(destroy_on_unload = true)
      ?(keep_alive_period = 25000)
      ?(long_poll_timeout = 60000)
      ~server
      () : properties =
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
  }

type response =
  { janus : string
  ; transaction : string
  ; session_id : int64 option [@default None]
  ; data : data option [@default None]
  ; error : error option [@default None]
  }
and error =
  { reason : string
  ; code : int
  }
and data =
  { id : int64
  } [@@deriving yojson { strict = false }]

let parse_response ~logs = function
  | Ok None ->
     logs.err (fun m -> m "%s" empty_response);
     Error empty_response
  | Error e ->
     let s = Api.error_to_string e in
     logs.err (fun m -> m "%s" s);
     Error s
  | Ok Some json ->
     logs.debug (fun m ->
         m "got response: %s"
         @@ Yojson.Safe.to_string json);
     match response_of_yojson json with
     | Error e ->
        logs.err (fun m -> m "Bad json format: %s" e);
        Error e
     | Ok ({ janus = "success"; _ } as rsp) -> Ok rsp
     | Ok { error = None; _ } ->
        let reason = "Unknown error" in
        logs.err (fun m -> m "%s" reason);
        Error reason
     | Ok { error = Some e; _ } ->
        logs.err (fun m -> m "Oops: %d %s" e.code e.reason);
        Error e.reason

let event_handler () : unit =
  (* FIXME implement *)
  ()

let on_create_success ~(logs : logs)
      ~reconnect
      ~on_create
      ~on_destroy
      (server : Uri.t)
      (rsp : response)
      (props : properties)
    : (t, string) Lwt_result.t =
  match rsp.data, rsp.session_id with
  | None, None ->
     let s = "No session ID found in response" in
     logs.err (fun m -> m "%s" s);
     Lwt_result.fail s
  | Some { id }, None | _, Some id ->
     let (t : t) =
       make ~id ~server ~props ~logs
         ~on_create ~on_destroy () in
     if reconnect
     then logs.debug (fun m -> m "Claimed session: %Ld" id)
     else logs.debug (fun m -> m "Created session: %Ld" id);
     event_handler ();
     on_create t;
     Lwt_result.return t

type reconnect =
  { server : Uri.t
  ; id : int64
  }

let rec create_session
          ~(logs : logs)
          ~(props : properties)
          ~on_create
          ~on_destroy
          ?remaining
          ?reconnect
          ()
        : (t, string) Lwt_result.t =
  (* TODO websockets *)
  let message =
    Message.make
      ?session_id:(Option.map (fun x -> x.id) reconnect)
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
  Api.http_api_call
    ~with_credentials:props.with_credentials
    ~body:message
    ~meth:`POST
    server
  >>= fun rsp ->
  match parse_response ~logs rsp with
  | Ok rsp ->
     on_create_success ~logs ~on_create ~on_destroy
       ~reconnect:(Option.is_some reconnect)
       server
       rsp
       props
  | Error e ->
     logs.err (fun m -> m "%s" e);
     (match snd props.server, remaining with
      | (_ :: _), Some [] ->
         "Error connecting to any of the provided Janus servers: \
          Is the server down?"
         |> Lwt_result.fail
      | (_ :: _), Some (hd :: tl) ->
         (* Let's try the next server *)
         Lwt_js.sleep 200.
         >>= (create_session ~on_create ~on_destroy
                ~remaining:(hd, tl) ~logs ~props)
      | [], _ | _, None -> Lwt_result.fail e)

let id (t : t) : int64 =
  t.id

let server (t : t) : Uri.t =
  t.server

let servers (t : t) : Uri.t list =
  let a, b = t.props.server in
  a :: b

let ice_servers (t : t) : Rtc_peer_connection.ICE.t list =
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

let attach_plugin (_ : t) : Plugin.t =
  failwith "XXX"

let destroy ?(async_request = true)
      ?(notify_destroyed = true) (t : t)
    : (unit, string) Lwt_result.t =
  t.logs.info (fun m -> m "destroying session %Ld (async=%b)"
                          t.id async_request);
  if not t.connected
  then (
    t.logs.warn (fun m -> m "Is the server down? (connected=false)");
    Lwt.return_ok ())
  else (
    let (message : Message.t Js.t) =
      Message.make
        ~janus:"destroy"
        ~transaction:(String.random 12)
        ?apisecret:t.props.apisecret
        ?token:t.props.token
        () in
    let (uri : Uri.t) =
      Printf.sprintf "%s/%Ld" (Uri.path t.server) t.id
      |> Uri.with_path t.server in
    Api.http_api_call ~meth:`POST
      ~async:async_request
      ~with_credentials:t.props.with_credentials
      ~body:message
      uri
    >|= (parse_response ~logs:t.logs)
    >|= fun x ->
    t.connected <- false;
    t.on_destroy t;
    if notify_destroyed then () (* FIXME *);
    match x with
    | Ok _ -> Ok ()
    | Error e -> Error e)

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
    ~logs:t.logs
    ~props
    ()
