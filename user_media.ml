open Js_of_ocaml
open Webrtc
open Adapter
open Utils
open Plugin_types
open Lwt_result.Infix

let get_screen_media ?(use_audio = false)
      (constraints : mediaStreamConstraints Js.t)
      (t : t) : (mediaStream Js.t, string) Lwt_result.t =
  Log.ign_info "Adding media constraint (screen capture)";
  Log.ign_debug ~inspect:constraints "";
  let fin res =
    Option.iter (fun f -> f false) t.on_consent_dialog;
    Lwt_result.lift res
    |> Lwt_result.map_err exn_to_string in
  let (media_devices : mediaDevices Js.t) =
    (Js.Unsafe.coerce Dom_html.window##.navigator)##.mediaDevices in
  Lwt.try_bind (fun () ->
      Promise.to_lwt @@ media_devices##getUserMedia constraints)
    (fun (stream : mediaStream Js.t) ->
      if use_audio
      then (
        let (c : mediaStreamConstraints Js.t) = Js.Unsafe.obj [||] in
        c##.video := wrap_bool false;
        c##.audio := wrap_bool true;
        Lwt.try_bind (fun () ->
            Promise.to_lwt @@ media_devices##getUserMedia c)
          (fun (audio : mediaStream Js.t) ->
            (match array_get audio##getAudioTracks 0 with
             | None -> ()
             | Some track -> stream##addTrack track);
            fin (Ok stream))
          (fun exn -> fin (Error exn)))
      else fin (Ok stream))
    (fun exn -> fin (Error exn))

let handle_screenshare (source : string)
      (frame_rate : int)
      (media_ext : Media.t_ext)
      (t : t) =
  let navigator = Js.Unsafe.coerce Dom_html.window##.navigator in
  if Js.Optdef.test navigator##.mediaDevices
     && Js.Optdef.test navigator##.mediaDevices##.getUserMedia
  then ();
  let (media_devices : mediaDevices Js.t) =
    (Js.Unsafe.coerce navigator)##.mediaDevices in
  if Js.Optdef.test (Js.Unsafe.coerce media_devices)##.getDisplayMedia
  then (
    (* The new experimental getDisplayMedia API is available, let's use that
					https://groups.google.com/forum/#!topic/discuss-webrtc/Uf0SrR4uxzk
					https://webrtchacks.com/chrome-screensharing-getdisplaymedia/ *)
    let (constraints : mediaStreamConstraints Js.t) = Js.Unsafe.obj [||] in
    constraints##.video := wrap_bool true;
    Lwt.try_bind (fun () ->
        Promise.to_lwt @@ media_devices##getDisplayMedia constraints)
      (fun (stream : mediaStream Js.t) ->
        Option.iter (fun f -> f false) t.on_consent_dialog;
        if Media.is_track_send_enabled media_ext.media.audio
           && not media_ext.keep_audio
        then (
          let (c : mediaStreamConstraints Js.t) = Js.Unsafe.obj [||] in
          c##.video := wrap_bool false;
          c##.audio := wrap_bool true;
          Lwt.try_bind (fun () ->
              Promise.to_lwt @@ media_devices##getUserMedia c)
            (fun (audio : mediaStream Js.t) ->
              (match array_get audio##getAudioTracks 0 with
               | None -> ()
               | Some track -> stream##addTrack track);
              Lwt.return_ok stream)
            (fun exn -> Lwt.return_error @@ exn_to_string exn))
        else Lwt.return_ok stream)
      (fun exn ->
        Option.iter (fun f -> f false) t.on_consent_dialog;
        Lwt.return_error @@ exn_to_string exn))
  else (
    (* We're going to try and use the extension for Chrome 34+,
          the old approach for older versions of Chrome, or the experimental
          support in Firefox 33+ *)
    if check_browser ~browser:"chrome" ()
    then (
      let user_agent = Dom_html.window##.navigator##.userAgent in
      let (is_linux : bool) =
        Js.Opt.test
        @@ user_agent##_match (new%js Js.regExp (Js.string "Linux")) in
      let ver = get_version () in
      (* "known" crash in chrome 34 and 35 on linux *)
      let maxver = if is_linux then 35 else 33 in
      if ver >= 26 && ver <= maxver
      then (
        (* Chrome 26->33 requires some awkward chrome://flags manipulation *)
        let mandatory = Js.Unsafe.(
            obj [| "googLeakyBucket", inject Js._true
                 ; "maxWidth", inject Dom_html.window##.screen##.width
                 ; "maxHeight", inject Dom_html.window##.screen##.height
                 ; "minFrameRate", inject frame_rate
                 ; "maxFrameRate", inject frame_rate
                 ; "chromeMediaSource", inject @@ Js.string "screen" |]) in
        let video = Js.Unsafe.(obj [|"mandatory", inject mandatory|]) in
        let audio =
          Js.Unsafe.inject
          @@ (Media.is_track_send_enabled media_ext.media.audio
              && not media_ext.keep_audio) in
        let constraints = Js.Unsafe.(
            obj [| "video", inject video
                 ; "audio", inject audio |]) in
        get_screen_media constraints t)
      else (
        (* Chrome 34+ requires an extension *)
        (* FIXME seems that extension is not working *)
        Lwt.return_error "This version of Chrome requires an extension, \
                          but this is not implemented"))
    else if check_browser ~browser:"firefox" ~ver:33 ~ver_cmp:(>=) ()
    then (
      (* Firefox 33+ has experimental support for screen sharing *)
      let video = Js.Unsafe.(
          obj [| "mozMediaSource", inject @@ Js.string source
               ; "mediaSource", inject @@ Js.string source |]) in
      let audio =
        Js.Unsafe.inject
        @@ Js.bool
        @@ (Media.is_track_send_enabled media_ext.media.audio
            && not media_ext.keep_audio) in
      let (c : mediaStreamConstraints Js.t) = Js.Unsafe.(
          obj [| "audio", inject audio
               ; "video", inject video |]) in
      get_screen_media c t
      >>= fun (stream : mediaStream Js.t) ->
      (* Workaround for https://bugzilla.mozilla.org/show_bug.cgi?id=1045810 *)
      let last_time = ref (Js.Unsafe.coerce stream)##.currentTime in
      let timer = ref None in
      let clear () = match !timer with
        | None -> ()
        | Some t -> Dom_html.window##clearInterval t in
      let cb = fun () ->
        let cur = (Js.Unsafe.coerce stream)##.currentTime in
        if not (Js.Optdef.test (Obj.magic stream))
           || not (Js.Opt.test (Obj.magic stream))
        then clear ()
        else if cur == !last_time
        then (
          clear ();
          if Js.Optdef.test (Js.Unsafe.coerce stream)##.onended
          then (Js.Unsafe.coerce stream)##onended ())
        else last_time := cur in
      timer := Some (Dom_html.window##setInterval (Js.wrap_callback cb) 500.);
      Lwt.return_ok stream)
    else if check_browser ~browser:"firefox" ()
    then (
      Option.iter (fun f -> f false) t.on_consent_dialog;
      Lwt.return_error "Your version of Firefox does not support screen \
                        sharing, please install Firefox 33 or later"
    )
    else
      Lwt.return_error "Nor chrome, nor Firefox, \
                        screen sharing is not supported")

let handle_media () =
  let (media_devices : mediaDevices Js.t) =
    (Js.Unsafe.coerce Dom_html.window##.navigator)##.mediaDevices in
  Lwt.try_bind (fun () -> Promise.to_lwt @@ media_devices##enumerateDevices)
    (fun devices ->
      let cb (kind : string) = fun (device : mediaDeviceInfo Js.t) _ _ ->
        Js.bool @@ String.equal (Js.to_string device##.kind) kind in
      let (audio_exists : bool) =
        Js.to_bool @@ devices##some (Js.wrap_callback (cb "audioinput")) in
      let (video_exists : bool) =
        Js.to_bool @@ devices##some (Js.wrap_callback (cb "videoinput")) in
      Lwt.return_ok ())
    (fun exn -> Lwt.return_error @@ exn_to_string exn)

let get_user_media_video (media_ext : Media.t_ext) (t : t) =
  match media_ext.media.video.typ with
  | Audio _ -> assert false
  | Video `Resolution res ->
     let width, height = Media.resolution_to_video_size res in
     ignore width; ignore height;
     Lwt.return_error ""
  | Video (`Screen fr as src) | Video (`Window fr as src) ->
     let fr = match fr with None -> 3 | Some x -> x in
     let source = match src with
       | `Screen _ -> "screen" | `Window _ -> "window" in
     handle_screenshare source fr media_ext t
  | _ -> Lwt.return_error ""

let get_user_media ~simulcast (media : Media.t) (t : t) =
  ignore simulcast;
  ignore media;
  Option.iter (fun f -> f true) t.on_consent_dialog;
  Lwt.return_error ""
