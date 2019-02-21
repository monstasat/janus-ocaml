open Js_of_ocaml
open Utils
open Types

type t =
  { logs : logs
  ; mutable close_listener : Dom_events.listener option
  ; mutable sessions : Session.t list
  }

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
      t.logs.info (fun m -> m "Closing window");
      List.iter (fun (s : Session.t) ->
          if s.props.destroy_on_unload
          then (
            t.logs.info (fun m -> m "Destroying session %s" @@ Session.show s);
            Session.destroy ~async_request:false ~notify_destroyed:false s))
        t.sessions;
      Option.iter (fun f -> Js.Unsafe.fun_call f [||]) old_bf;
      true)

let create ?log_level () : t =
  let src = Logs.Src.create "janus" in
  Logs.Src.set_level src log_level;
  Logs.set_reporter @@ Logs_browser.console_reporter ();
  let (module Logs) = Logs.src_log src in
  let (logs : logs) =
    { app = Logs.app
    ; err = Logs.err
    ; warn = Logs.warn
    ; info = Logs.info
    ; debug = Logs.debug
    } in
  logs.info (fun m -> m "Initializing library");
  let t =
    { logs
    ; sessions = []
    ; close_listener = None
    } in
  { t with close_listener = Some (detect_tab_close t) }

let create_session (t : t) (props : Session.properties)
    : (Session.t, string) Lwt_result.t =
  Lwt_result.(
    Session.create_session ~logs:t.logs ~props ()
    >|= (fun session ->
      t.sessions <- List.cons session t.sessions;
      session))

(* FIXME legacy *)
let init ?log_level () : unit Lwt.t =
  ignore log_level;
  let t = create ~log_level:Debug () in
  let server = Uri.of_string "http://127.0.0.1:8088/janus", [] in
  let s = create_session t (Session.make_properties ~server ()) in
  ignore s;
  let inject = Js.Unsafe.inject in
  let t, w = Lwt.wait () in
  [| ("callback",
      inject
      @@ Js.wrap_callback
      @@ fun () -> Lwt.wakeup w ())
  |]
  |> (fun x -> ignore @@ Js.Unsafe.(fun_call (js_expr "Janus.init") [| obj x |]));
  t
