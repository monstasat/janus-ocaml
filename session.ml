open Utils
open Lwt.Infix
open Types

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
  ; logs : logs
  }

let make ?(connected = true) ?(plugins = []) ~logs ~id ~server ~props () : t =
  { id; server; connected; plugins; props; logs }

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

type create_response =
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
  | Ok None -> Error "Response is empty"
  | Ok Some json ->
     logs.debug (fun m -> m "%s" @@ Yojson.Safe.to_string json);
     create_response_of_yojson json
  | Error ((_, Some _) as e) ->
     let msg = Api.error_to_string e in
     logs.err (fun m -> m "%s" msg);
     Error msg
  | Error ((m, None) as e) ->
     logs.err (fun m -> m "%s" @@ Api.error_to_string e);
     Error (m ^ ": Is the gateway down?")


let event_handler () : unit =
  (* FIXME implement *)
  ()

let on_success ~(logs : logs)
      ~reconnect
      (server : Uri.t)
      (rsp : create_response)
      (props : properties)
    : (t, string) Lwt_result.t =
  match rsp.data, rsp.session_id with
  | None, None ->
     let s = "No session ID found in response" in
     logs.err (fun m -> m "%s" s);
     Lwt_result.fail s
  | Some { id }, None | _, Some id ->
     let (t : t) = make ~id ~server ~props ~logs () in
     if reconnect
     then logs.debug (fun m -> m "Claimed session: %Ld" id)
     else logs.debug (fun m -> m "Created session: %Ld" id);
     event_handler ();
     Lwt_result.return t

let on_error ~(logs : logs) (rsp : create_response) : string =
  match rsp.error with
  | None ->
     let reason = "Unknown error" in
     logs.err (fun m -> m "%s" reason);
     reason
  | Some e ->
     logs.err (fun m -> m "Oops: %d %s" e.code e.reason);
     e.reason

type reconnect =
  { server : Uri.t
  ; id : int64
  }

let rec create_session
          ~(logs : logs)
          ~(props : properties)
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
    (Uri.to_string server)
  >>= fun rsp ->
  match parse_response ~logs rsp with
  | Ok (rsp : create_response) ->
     (match rsp.janus with
      | "success" ->
         on_success ~logs
           ~reconnect:(Option.is_some reconnect)
           server
           rsp
           props
      | _ -> Lwt_result.fail (on_error ~logs rsp))
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
         >>= (create_session ~remaining:(hd, tl) ~logs ~props)
      | [], _ | _, None -> Lwt_result.fail e)

let show (t : t) : string =
  ignore t;
  ""

let id (t : t) : int64 =
  t.id

let server (t : t) : Uri.t =
  t.server

let connected (t : t) : bool =
  t.connected

let attach (t : t) : Plugin.t =
  ignore t;
  failwith "implement me"

let destroy ?async_request ?notify_destroyed (t : t) : unit =
  ignore t; ignore async_request; ignore notify_destroyed;
  ()

let reconnect (props : properties)
      (({ id; server; logs; _ } as t) : t)
    : (t, string) Lwt_result.t =
  t.connected <- false;
  let reconnect = { id; server } in
  create_session ~reconnect ~logs ~props ()
