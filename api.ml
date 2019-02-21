open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Infix
open Utils

type error =
  { code : int
  ; error : string
  }

let error_to_string (e : error) : string =
  match e.error with
  | "" -> Printf.sprintf "Unknown error, code = %d" e.code
  | s -> s

let make_error ?(error = "") ~code () =
  { code; error }

let parse_response (rsp : 'a Js.opt Lwt_xmlHttpRequest.generic_http_frame) =
  match Js.Opt.to_option rsp.content with
  | None -> Error (make_error ~code:0 ~error:"empty response" ())
  | Some s ->
     (try Ok s
      with e -> Error (make_error ~code:0 ~error:(Printexc.to_string e) ()))

let http_api_call ?timeout ?with_credentials
      ?(body : Jsobj.t option)
      ~meth
      url =
  let content_type = match meth with
    | `POST -> Some ("Content-Type", "application/json")
    | _ -> None in
  let (headers : (string * string) list) =
    ["Accept", "application/json, text/plain, */*"]
    |> List.cons_maybe content_type in
  let contents = match body with
    | None -> None
    | Some b ->
       let s = Js.to_string @@ Json.output @@ Js.Unsafe.obj b in
       Some (`String s) in
  let t =
    Lwt_xmlHttpRequest.perform_raw
      ?with_credentials
      ?contents
      ~headers
      ~override_method:meth
      ~response_type:XmlHttpRequest.JSON
      url
    >|= fun x ->
    Printf.printf "got response: %d\n" x.code;
    Js.Unsafe.global##.console##log x.content |> ignore;
    match x.code with
    | 200 -> parse_response x
    | code -> Error (make_error ~code ()) in
  match timeout with
  | None -> t
  | Some (x : float) ->
     [ Lwt_js.sleep (x /. 1000.)
       >|= (fun () -> Error (make_error ~error:"Request timed out" ~code:0 ()))
     ; t
     ]
     |> Lwt.choose
