# Janus-ocaml

Janus-ocaml is a library for manipulating [Janus WebRTC server](https://janus.conf.meetecho.com).
For now, it is only possible to control Janus server via HTTP with `janus-js` lib.

## Janus-js

A library to ease task of creating sessions with the Janus core, 
attaching WebRTC users to plugins, send and receive requests to the plugins
and receive events from them an so on.
Please notice that `janus-js` library makes use of the features made available
by the [webrtc-adapter](https://github.com/webrtc/adapter) shim, which means that your web applications should
always include it as a dependency, like:

``` html
<script src="https://cdnjs.cloudflare.com/ajax/libs/webrtc-adapter/6.4.0/adapter.min.js" type="text/javascript"></script>
```

### Usage

``` ocaml
open Js_of_ocaml

let demo (video : Dom_html.videoElement Js.t) =
  let janus = Janus_js.create ~log_level:Debug () in
  Lwt.Infix.(
    Janus_js.create_session
      ~server:"http://localhost:8088/janus"
      ~on_error:(fun (e : string) -> print_endline e)
      janus
    >>= function
    | Error e -> Lwt.return_error e
    | Ok (session : Janus_js.Session.t) ->
       Janus_js.Session.attach_plugin
         ~typ:Streaming
         ~on_remote_stream:(fun (stream : Janus_js.Webrtc.mediaStream Js.t)
                                (_ : Janus_js.Plugin.t) ->
           Janus_js.attach_media_stream video stream)
         ~on_message:(fun ?jsep _ (plugin : Janus_js.Plugin.t) ->
           match jsep with
           | None -> ()
           | Some jsep ->
              match Js.to_string jsep##._type with
              | "answer" ->
                 Janus_js.Plugin.handle_remote_jsep jsep plugin
                 |> Lwt.ignore_result
              | "offer" ->
                 (let video = Janus_js.Media.make_video ~source:(`Bool false) () in
                  let audio = Janus_js.Media.make_audio ~source:(`Bool false) () in
                  let media = Janus_js.Media.make ~audio ~video () in
                  Janus_js.Plugin.create_answer ~jsep ~data:(`Bool true) (`Media media) plugin
                  >|= function
                  | Ok _ ->
                     (* Do something, e.g. send "start" message to the streaming plugin*)
                     ()
                  | Error e -> print_endline e)
                 |> Lwt.ignore_result
              | s -> Printf.printf "Unknown jsep received: %s\n" s)
         session
       >>= function
       | Error e -> Lwt.return_error e
       | Ok (plugin : Janus_js.Plugin.t) ->
          (* Do something with plugin*)
          Lwt.return_ok plugin)
```

### Janus-js TODO

* Add sublibraries for Janus plugins
* Think about parameterized plugin type. Dependent on the plugin type,
message parsers and serializers can be defined
