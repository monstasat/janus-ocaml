open Js_of_ocaml

exception Not_created of string

let ( >|= ) x f = Js.Optdef.map x f
let ( >>= ) = Lwt.( >>= )
let ( % ) a b c = a (b c)

let is_some = function None -> false | _ -> true

let int_of_number x = int_of_float @@ Js.float_of_number x
let wrap_js_optdef x f = Js.Optdef.option x >|= f |> Js.Unsafe.inject

let wakeup_exn (w : 'a Lwt.u) (s : string) : unit =
  Lwt.wakeup_exn w (Not_created s)

let call_js_method f params =
  let open Js.Unsafe in
  let t, w = Lwt.wait () in
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

  type e = string

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

  let typ_to_string type_ =
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
    [|media'; trickle'|]
    |> (fun a -> if is_some jsep then Array.append a [|jsep'|] else a)

  (* Plugin functions *)

  let get_id plugin = plugin##getId () |> Js.float_of_number |> Int64.of_float

  let get_name plugin = plugin##getPlugin () |> Js.to_string

  (* FIXME use call_js_method inside?? *)
  let send ?jsep (plugin : t) request request_to_string
        request_to_params parse_response =
    let open Js.Unsafe in
    let t, w = Lwt.wait () in
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

  let create_answer (plugin : t) media trickle jsep =
    prepare_offer_or_answer ~jsep:jsep media trickle
    |> call_js_method (fun x -> plugin##createAnswer x)

  let create_offer (plugin : t) media trickle =
    prepare_offer_or_answer media trickle
    |> call_js_method (fun x -> plugin##createAnswer x)

  let handle_remote_jsep (plugin : t) jsep =
    call_js_method (fun x -> plugin##handleRemoteJsep x)
      [| ("jsep", Js.Unsafe.inject jsep) |]

  let dtmf (plugin : t) tones duration gap =
    let open Js.Unsafe in
    let tones' = ("tones", inject @@ Js.string tones) in
    let duration' = ("duration", wrap_js_optdef duration (fun x -> x)) in
    let gap' = ("gap", wrap_js_optdef gap (fun x -> x)) in
    call_js_method (fun x -> plugin##dtmf x)
      [| tones'; duration'; gap' |]

  let data (plugin : t) text =
    let text' = ("text", Js.Unsafe.inject @@ Js.string text) in
    call_js_method (fun x -> plugin##data x) [| text' |]

  let get_bitrate (plugin : t) =
    plugin##getBitrate () |> Js.to_string

  let hangup (plugin : t) send_request =
    plugin##hangup (Js.bool send_request)

  let detach (plugin : t) =
    call_js_method (fun x -> plugin##detach x) [| |]

end

module Session = struct

  type t = Janus.janus Js.t

  type e =
    | Err of string
    | Destroyed

  type jsep =
    | Offer of Js.json Js.t
    | Answer of Js.json Js.t
    | Unknown of Js.json Js.t

  let handle_message (plugin : Plugin.t Lwt.t) on_msg on_jsep msg jsep =
    match Lwt.poll plugin with
    | None ->
       (* Unreachable case since promise should be fullfilled by the time
          plugin starts sending messages *)
       assert false
    | Some (plugin : Plugin.t) ->
       let jsep' =
         Js.Optdef.to_option jsep
         |> function None -> None
                   | Some x -> Js.Opt.to_option x in
       (* Handle message *)
       (match on_msg with
        | None -> ()
        | Some f -> f plugin msg);
       (* Handle jsep if some *)
       (match jsep', on_jsep with
        | Some jsep, Some f ->
           let jsep = match Js.to_string (Js.Unsafe.coerce jsep)##.type_ with
             | "offer" -> Offer jsep
             | "answer" -> Answer jsep
             | _ -> Unknown jsep in
           f plugin jsep
        | _ -> ())

  let get_server (session : t) : string =
    Js.to_string @@ session##getServer ()

  let is_connected (session : t) : bool =
    Js.to_bool @@ session##isConnected ()

  let get_session_id (session : t) : int64 =
    Int64.of_float @@ Js.float_of_number @@ session##getSessionId ()

  let wrap_cb = fun ?f name f_push ->
    name,
    match f with
    | None -> wrap_js_optdef f_push Js.wrap_callback
    | Some f -> wrap_js_optdef f_push (fun push ->
                    Js.wrap_callback (fun data -> push @@ f data))

  let attach ~session ~typ ?opaque_id ?on_local_stream
        ?on_remote_stream ?on_message ?on_jsep
        ?consent_dialog ?webrtc_state ?ice_state
        ?media_state ?slow_link ?on_cleanup ?detached ()
      : (Plugin.t * Plugin.e React.event) Lwt.t =
    let inject = Js.Unsafe.inject in
    let t, w = Lwt.wait () in
    let e, set_e = React.E.create () in
    let on_error = fun e ->
      let s = Js.to_string e in
      if Lwt.is_sleeping t
      then wakeup_exn w s
      else set_e s in
    let on_message' = handle_message t on_message on_jsep in
    let to_media_state = fun (s, b) -> Js.(to_string s, to_bool b) in
    [| ("plugin", inject @@ Js.string @@ Plugin.typ_to_string typ)
     ; ("opaqueId", wrap_js_optdef opaque_id Js.string)
     ; ("onmessage", inject @@ Js.wrap_callback on_message')
     ; wrap_cb "consentDialog" consent_dialog ~f:Js.to_bool
     ; wrap_cb "webrtcState" webrtc_state ~f:Js.to_bool
     ; wrap_cb "iceState" ice_state ~f:Js.to_string
     ; wrap_cb "mediaState" media_state ~f:to_media_state
     ; wrap_cb "slowLink" slow_link ~f:Js.to_bool
     ; wrap_cb "oncleanup" on_cleanup
     ; wrap_cb "detached" detached
     ; wrap_cb "onlocalstream" on_local_stream
     ; wrap_cb "onremotestream" on_remote_stream
     ; ("success", inject @@ Js.wrap_callback (Lwt.wakeup w))
     ; ("error", inject @@ Js.wrap_callback on_error)
    |]
    |> Js.Unsafe.obj
    |> (fun obj -> session##attach obj);
    Lwt.(t >|= (fun t -> t, e))

  let destroy (session : t) : ('a, string) Lwt_result.t =
    call_js_method (fun x -> session##destroy x) [||]

end

type debug_token =
  | Trace
  | Debug
  | Log
  | Warn
  | Error

let debug_token_to_string : debug_token -> string = function
  | Trace -> "trace"
  | Debug -> "debug"
  | Log -> "log"
  | Warn -> "warn"
  | Error -> "error"

let to_js_string_array = Js.array % Array.of_list % List.map Js.string

let create ~server ?ice_servers ?ipv6 ?with_credentials
      ?max_poll_events ?destroy_on_unload ?token ?apisecret ()
    : (Session.t * Session.e React.event) Lwt.t =
  let inject = Js.Unsafe.inject in
  let t, w = Lwt.wait () in
  let (e : Session.e React.event), set_e = React.E.create () in
  let server = match server with
    | `One x -> inject @@ Js.string x
    | `Many x -> inject @@ to_js_string_array x in
  let on_error = fun e ->
    let s = Js.to_string e in
    print_endline s;
    if Lwt.is_sleeping t
    then wakeup_exn w s
    else set_e (Err s) in
  let j =
    [| ("server", server)
     ; ("iceServers", wrap_js_optdef ice_servers to_js_string_array)
     ; ("ipv6", wrap_js_optdef ipv6 Js.bool)
     ; ("withCredentials", wrap_js_optdef with_credentials Js.bool)
     ; ("max_poll_events", wrap_js_optdef max_poll_events (fun x -> x))
     ; ("destroyOnUnload", wrap_js_optdef destroy_on_unload Js.bool)
     ; ("token", wrap_js_optdef token Js.string)
     ; ("apisecret", wrap_js_optdef apisecret Js.string)
     ; ("success", inject @@ Js.wrap_callback (Lwt.wakeup w))
     ; ("error", inject @@ Js.wrap_callback on_error)
     ; ("destroy", inject @@ Js.wrap_callback (fun () -> set_e Destroyed))
    |]
    |> Janus.create in
  t >>= (fun () -> Lwt.return (j, e))

let init debug =
  let inject = Js.Unsafe.inject in
  let t, w = Lwt.wait () in
  [| ("debug",
      match debug with
      | `All x -> inject @@ Js.bool x
      | `Several x ->
         inject
         @@ to_js_string_array
         @@ List.map debug_token_to_string x)
   ; ("callback",
      inject
      @@ Js.wrap_callback
      @@ fun () ->
         if Janus.isWebrtcSupported ()
         then Lwt.wakeup w ()
         else Lwt.wakeup_exn w (Failure "WebRTC is not supported"))
  |]
  |> Janus.init;
  t
