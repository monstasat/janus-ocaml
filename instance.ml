open Js_of_ocaml
open Utils
open Types

type t =
  { mutable close_listener : Dom_events.listener option
  ; mutable sessions : (int64 * Session.t) list
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
      t.logs.info.str "Closing window";
      List.iter (fun (id, (s : Session.t)) ->
          if Session.destroy_on_unload s
          then (
            t.logs.info.str @@ Printf.sprintf "Destroying session %Ld" id;
            Session.destroy ~async_request:false ~notify_destroyed:false s
            |> Lwt.ignore_result))
        t.sessions;
      Option.iter (fun f -> Js.Unsafe.fun_call f [||]) old_bf;
      true)

let create ?(log_level = Logs.Error) () : t =
  Logs.Section.set_level Logs.section log_level;
  Logs.ign_log "Initializing library";
  let t =
    { logs
    ; sessions = []
    ; close_listener = None
    } in
  { t with close_listener = Some (detect_tab_close t) }

let create_session (t : t) (props : Session.properties)
    : (Session.t, string) Lwt_result.t =
  let on_create = fun (s : Session.t) ->
    let id = Session.id s in
    t.sessions <- List.set_assoc ~eq:Int64.equal id s t.sessions in
  let on_destroy = fun (s : Session.t) ->
    let id = Session.id s in
    t.sessions <- List.remove_assoc id t.sessions in
  Session.create_session ~on_create ~on_destroy ~logs:t.logs ~props ()

(* FIXME legacy *)
let init ?(log_level : Logs.level option) () : unit Lwt.t =
  ignore log_level;
  Lwt_result.(
    let t = create ~log_level:Debug () in
    let server = Uri.of_string "http://127.0.0.1:8088/janus", [] in
    create_session t (Session.make_properties ~server ())
    >>= (Session.attach_plugin ~plugin:"janus.plugin.streaming"))
  |> Lwt.ignore_result;
  let inject = Js.Unsafe.inject in
  let t, w = Lwt.wait () in
  [| ("callback",
      inject
      @@ Js.wrap_callback
      @@ fun () -> Lwt.wakeup w ())
  |]
  |> (fun x -> ignore @@ Js.Unsafe.(fun_call (js_expr "Janus.init") [| obj x |]));
  t
