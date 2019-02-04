open Js_of_ocaml

exception Not_created of string

type 'a janus_result = ('a, string) Result.result Lwt.t

let ( >|= ) x f = Js.Optdef.map x f
let ( >>= ) = Lwt.( >>= )

let opt_bind f = function
  | Some x -> f x
  | None   -> None

let opt_map f = function
  | Some x -> Some (f x)
  | None   -> None

let is_some = function None -> false | _ -> true

let int_of_number x = int_of_float @@ Js.float_of_number x
let wrap_js_optdef x f = Js.Optdef.option x >|= f |> Js.Unsafe.inject
let bind_undef_or_null x f =
  Js.Optdef.to_option x
  |> opt_bind Js.Opt.to_option
  |> opt_bind f

let call_js_method f params =
  let t,w     = Lwt.wait () in
  let open Js.Unsafe in
  let success =
    ("success",
     inject @@ Js.wrap_callback (fun x -> Lwt.wakeup w x)) in
  let error =
    ("error",
     inject @@ Js.wrap_callback (fun err ->
                   Lwt.wakeup_exn w (Failure (Js.to_string (Json.output err))))) in
  f (obj @@ Array.append params [| success; error |]);
  Lwt.catch
    (fun () -> t >>= (fun x -> Lwt.return_ok x))
    (function
     | Failure e -> Lwt.return_error e
     | e -> Lwt.return_error (Printexc.to_string e))

(** Janus plugin handler **)
module Plugin = struct

  (* Types *)

  type t = Janus.plugin Js.t

  type plugin_type =
    | Audiobridge
    | Echotest
    | Recordplay
    | Sip
    | Streaming
    | Textroom
    | Videocall
    | Videoroom
    | Voicemail

  type media_video =
    | Bool of bool
    | Resolution of ([`Lowres | `Stdres | `Hires] * [`Wide | `Square ])
    | Screen
    | Device of (int * int * int)

  type media_audio =
    | Bool of bool
    | Device of int

  type media_props =
    { audio_send : bool option
    ; audio_recv : bool option
    ; audio : media_audio option
    ; video_send : bool option
    ; video_recv : bool option
    ; video : media_video option
    ; data : bool option
    ; fail_if_no_video : bool option
    ; fail_if_no_audio : bool option
    ; screen_rate : int option
    }

  type 'a send_response =
    | Error of int option * string
    | Empty
    | Data of 'a

  (* Helper functions *)

  let plugin_type_to_string type_ =
    let name = match type_ with
      | Audiobridge -> "audiobridge"
      | Echotest -> "echotest"
      | Recordplay -> "recordplay"
      | Sip -> "sip"
      | Streaming -> "streaming"
      | Textroom -> "textroom"
      | Videocall -> "videocall"
      | Videoroom -> "videoroom"
      | Voicemail -> "voicemail" in
    "janus.plugin." ^ name

  let get_resolution_string quality aspect =
    let q = match quality with
      | `Lowres -> "lowres"
      | `Stdres -> "stdres"
      | `Hires -> "hires" in
    let a = match aspect with
      | `Wide -> "-16:9"
      | `Square -> "" in
    q ^ a

  let parse_sync_response name f = function
    | Empty -> Result.Error "Empty message"
    | Data d ->
       let prop = Js.Unsafe.get d name in
       Js.Optdef.bind prop (fun x -> Js.Unsafe.get d x)
       |> Js.Optdef.to_option
       |> (function
           | Some x -> f x
           | None -> Result.Error "Bad response")
    | Error (_, msg) -> Result.Error msg

  let parse_async_response = function
    | Empty -> Ok ()
    | Error (_, msg) -> Result.Error msg (* FIXME add error code to error string? *)
    | Data _ -> Result.Error "Data returned in async request"

  let data_or_error response =
    let response = Js.Optdef.to_option response in
    match response with
    | Some response ->
      if (Js.Optdef.test ((Js.Unsafe.coerce response)##.error_code)) ||
         (Js.Optdef.test ((Js.Unsafe.coerce response)##.error))
      then
        let error = match Js.Optdef.to_option (Js.Unsafe.coerce response)##.error with
          | Some x -> Js.to_string x
          | None -> "" in
        let code  = match Js.Optdef.to_option (Js.Unsafe.coerce response)##.error_code with
          | Some x -> Some (int_of_number x)
          | None -> None in
        Error (code, error)
      else Data response
    | None -> Empty

   let prepare_offer_or_answer ?jsep media trickle =
    let open Js.Unsafe in
    let media' =
      ("media",
       [ ("audioSend", wrap_js_optdef media.audio_send Js.bool);
         ("audioRecv", wrap_js_optdef media.audio_recv Js.bool);
         ("audio",
          match media.audio with
          | Some x ->
             begin match x with
             | Bool x -> Js.bool x |> inject
             | Device id -> obj [| ("deviceId", Js.Unsafe.inject id) |] |> inject
             end
          | None -> inject Js.Optdef.empty);
         ("videoSend", wrap_js_optdef media.video_send Js.bool);
         ("videoRecv", wrap_js_optdef media.video_recv Js.bool);
         ("video",
          match media.video with
          | Some x ->
             begin match x with
             | Bool x -> Js.bool x |> inject
             | Resolution (quality, aspect) ->
                get_resolution_string quality aspect |> Js.string |> inject
             | Screen -> Js.string "screen" |> Js.Unsafe.inject
             | Device (id, w, h) ->
                obj [| ("deviceId", inject id);
                       ("width", inject w);
                       ("height", inject h) |] |> inject
             end
          | None -> Js.Unsafe.inject Js.Optdef.empty);
         ("data", wrap_js_optdef media.data Js.bool);
         ("failIfNoVideo", wrap_js_optdef media.fail_if_no_video Js.bool);
         ("failIfNoAudio", wrap_js_optdef media.fail_if_no_audio Js.bool);
         ("screenShareFrameRate", wrap_js_optdef media.screen_rate (fun x -> x)) ]
       |> Array.of_list |> obj |> inject ) in
    let trickle' = ("trickle", wrap_js_optdef trickle Js.bool) in
    let jsep' = ("jsep", wrap_js_optdef jsep (fun x -> x)) in
    [| media'; trickle' |]
    |> (fun a -> if is_some jsep then Array.append a [| jsep' |] else a)

   (* Plugin functions *)

   let get_id plugin = plugin##getId () |> Js.float_of_number |> Int64.of_float

   let get_name plugin = plugin##getPlugin () |> Js.to_string

   (* FIXME use call_js_method inside?? *)
   let send ?jsep (plugin:t) request request_to_string request_to_params parse_response =
     let t,w     = Lwt.wait () in
     let open Js.Unsafe in
    let request' = ("request", request_to_string request |> Js.string |> inject) in
    let params = request_to_params request in
    let message = ("message", inject @@ obj @@ Array.append [| request' |] params) in
    let jsep' = ("jsep", inject @@ Js.Optdef.option jsep) in
    let success = ("success", inject @@ Js.wrap_callback (fun x -> Lwt.wakeup w x)) in
    let error = ("error", inject @@ Js.wrap_callback (fun err ->
        Lwt.wakeup_exn w (Failure (Js.to_string (Json.output err))))) in
    plugin##send (obj  [| request'; message; jsep'; success; error |]);
    Lwt.catch
      (fun () -> t >>= (fun x -> Lwt.return @@ parse_response x request))
      (function
        | Failure e -> Lwt.return_error e
        | e -> Lwt.return_error (Printexc.to_string e))

  let create_answer (plugin:t) media trickle jsep =
    prepare_offer_or_answer ~jsep:jsep media trickle
    |> call_js_method (fun x -> plugin##createAnswer x)

  let create_offer (plugin:t) media trickle =
    prepare_offer_or_answer media trickle
    |> call_js_method (fun x -> plugin##createAnswer x)

  let handle_remote_jsep (plugin:t) jsep =
    call_js_method (fun x -> plugin##handleRemoteJsep x)
      [| ("jsep", Js.Unsafe.inject jsep) |]

  let dtmf (plugin:t) tones duration gap =
    let open Js.Unsafe in
    let tones' = ("tones", inject @@ Js.string tones) in
    let duration' = ("duration", wrap_js_optdef duration (fun x -> x)) in
    let gap' = ("gap", wrap_js_optdef gap (fun x -> x)) in
    call_js_method (fun x -> plugin##dtmf x)
      [| tones'; duration'; gap' |]

  let data (plugin:t) text =
    let text' = ("text", Js.Unsafe.inject @@ Js.string text) in
    call_js_method (fun x -> plugin##data x) [| text' |]

  let get_bitrate (plugin:t) =
    plugin##getBitrate () |> Js.to_string

  let hangup (plugin:t) send_request =
    plugin##hangup (Js.bool send_request)

  let detach (plugin:t) =
    call_js_method (fun x -> plugin##detach x) [| |]

end

module Session = struct

  type t = Janus.janus Js.t

  type jsep =
    | Offer of Js.json Js.t
    | Answer of Js.json Js.t
    | Unknown of Js.json Js.t

  type 'a react_push = ('a -> unit)

  let handle_message (msg,jsep,on_msg,on_jsep) =
    (* Handle message *)
    opt_map (fun f -> f msg) on_msg |> ignore;
    (* Handle jsep if some *)
    bind_undef_or_null jsep (fun jsep' ->
        let jsep_type = (Js.to_string (Js.Unsafe.coerce jsep')##.type_) in
        opt_map (fun f -> f (if String.equal jsep_type "offer"
                             then Offer jsep'
                             else if String.equal jsep_type "answer"
                             then Answer jsep'
                             else Unknown jsep'))
          on_jsep |> (fun x -> Some x))


  let get_server (session:t) =
    session##getServer () |> Js.to_string

  let is_connected (session:t) =
    session##isConnected () |> Js.to_bool

  let get_session_id (session:t) =
    session##getSessionId () |> Js.float_of_number |> Int64.of_float

  let attach ~session ~plugin_type ?opaque_id ?on_local_stream
        ?on_remote_stream ?on_message ?on_jsep
        ?consent_dialog ?webrtc_state ?ice_state
        ?media_state ?slow_link ?on_cleanup ?detached () =
    let t, w = Lwt.wait () in

    let open Js.Unsafe in
    let wrap_cb           = fun ?f name f_push ->
      (name, match f with
        | Some f -> wrap_js_optdef f_push (fun push -> Js.wrap_callback (fun data -> push @@ f data))
        | None   -> wrap_js_optdef f_push Js.wrap_callback) in
    let plugin_name =
      ("plugin",
       Plugin.plugin_type_to_string plugin_type
       |> Js.string |> inject) in
    let opaque_id' =
      ("opaqueId",
       wrap_js_optdef opaque_id Js.string) in
    let success =
      ("success",
       Js.wrap_callback (fun plugin -> Lwt.wakeup w plugin)
       |> inject) in
    let error =
      ("error",
       Js.wrap_callback (fun s ->
           Lwt.wakeup_exn w (Not_created (Js.to_string s)))
       |> inject) in
    let consent_dialog' =
      wrap_cb "consentDialog" consent_dialog ~f:Js.to_bool in
    let webrtc_state' =
      wrap_cb "webrtcState" webrtc_state ~f:Js.to_bool in
    let ice_state' =
      wrap_cb "iceState" ice_state ~f:Js.to_string in
    let media_state' =
      wrap_cb "mediaState" media_state
        ~f:(fun (s, b) -> (Js.to_string s, Js.to_bool b)) in
    let slow_link' =
      wrap_cb "slowLink" slow_link ~f:Js.to_bool in
    let on_cleanup' =
      wrap_cb "oncleanup" on_cleanup in
    let detached'  =
      wrap_cb "detached" detached in
    let on_local_stream' =
      wrap_cb "onlocalstream" on_local_stream in
    let on_remote_stream' =
      wrap_cb "onremotestream" on_remote_stream in
    let on_message' =
      ("onmessage", (Js.wrap_callback (fun m j ->
                         handle_message (m, j, on_message, on_jsep)))
                    |> inject) in
    let params =
      [| plugin_name
       ; opaque_id'
       ; on_message'
       ; consent_dialog'
       ; webrtc_state'
       ; ice_state'
       ; media_state'
       ; slow_link'
       ; on_cleanup'
       ; detached'
       ; on_local_stream'
       ; on_remote_stream'
       ; success; error
      |] in
    let () = session##attach (obj params) in
    t

  let destroy (session:t) =
    call_js_method (fun x -> session##destroy x) [||]

end

type debug_token =
  | Trace
  | Debug
  | Log
  | Warn
  | Error

let create ~server ?ice_servers ?ipv6 ?with_credentials
    ?max_poll_events ?destroy_on_unload ?token ?apisecret () =
  let open Js.Unsafe in
  let t, w = Lwt.wait () in
  let destr_t, destr_w = Lwt.wait () in
  let server' =
    ("server",
     (match server with
      | `One x -> Js.string x |> inject
      | `Many x -> (List.map Js.string x |> Array.of_list
                    |> Js.array |> inject))) in
  let ice_servers' =
    ("iceServers",
     wrap_js_optdef ice_servers
       (fun l -> List.map Js.string l |> Array.of_list)) in
  let ipv6' =
    ("ipv6",
     wrap_js_optdef ipv6 Js.bool) in
  let with_credentials' =
    ("withCredentials",
     wrap_js_optdef with_credentials Js.bool) in
  let max_poll_events' =
    ("max_poll_events",
     wrap_js_optdef max_poll_events (fun x -> x)) in
  let destroy_on_unload' =
    ("destroyOnUnload",
     wrap_js_optdef destroy_on_unload Js.bool) in
  let token' = ("token", wrap_js_optdef token Js.string) in
  let apisecret' =
    ("apisecret",
     wrap_js_optdef apisecret Js.string) in
  let success =
    ("success",
     Js.wrap_callback (fun () -> Lwt.wakeup w ()) |> inject) in
  let error =
    ("error",
     Js.wrap_callback (fun s ->
         Lwt.wakeup_exn w (Not_created (Js.to_string s)))
     |> inject) in
  let destroy =
    ("destroy",
     Js.wrap_callback (fun () -> Lwt.wakeup destr_w ()) |> inject) in
  let j = Janus.create [| server'
                        ; ice_servers'
                        ; ipv6'
                        ; with_credentials'
                        ; max_poll_events'
                        ; destroy_on_unload'
                        ; token'; apisecret'
                        ; success
                        ; error
                        ; destroy |] in
  t >>= (fun () -> Lwt.return j), destr_t

let init debug =
  let open Js.Unsafe in
  let debug_token_to_string = function
    | Trace -> "trace" | Debug -> "debug" | Log -> "log"
    | Warn  -> "warn"  | Error -> "error" in
  let thread, wakener = Lwt.wait () in
  let debug' =
    ("debug",
     match debug with
     | `All x -> Js.bool x |> inject
     | `Several x ->
        List.map (fun l -> debug_token_to_string l |> Js.string) x
        |> Array.of_list |> Js.array |> inject) in
  let cb =
    ("callback",
     (fun () -> (if Janus.isWebrtcSupported ()
                 then Lwt.wakeup wakener ()
                 else Lwt.wakeup_exn wakener (Failure "WebRTC is not supported")))
     |> Js.wrap_callback |> inject) in
  Janus.init [| debug'; cb |];
  thread
