open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Infix
open Utils

type ok = Yojson.Safe.json option

type error = string * error_ext option
and error_ext =
  { response : string option
  ; error : string option
  ; timeout : float option
  }

let make_error ?timeout ?error ?response ?message ()
    : ('a, error) result =
  let message = match message with
    | None | Some "" -> "<< internal error >>"
    | Some s -> s in
  let ext = match timeout, error, response with
    | None, None, None -> None
    | _ -> Some { response; error; timeout } in
  Error (message, ext)

let error_to_string ((text, ext) : error) : string =
  let option_to_string k f v = match v with
    | None -> ""
    | Some r -> Printf.sprintf "%s: %s" k (f r) in
  match ext with
  | None -> Printf.sprintf "%s" text
  | Some { response; error; timeout } ->
     let details =
       String.concat "; "
         [ option_to_string "response" (fun x -> x) response
         ; option_to_string "error" (fun x -> x) error
         ; option_to_string "timeout" (Printf.sprintf "%g") timeout ] in
     Printf.sprintf "%s: {%s}" text details

let parse_response (rsp : Js.js_string Js.t Lwt_xmlHttpRequest.generic_http_frame) =
  match Js.to_string rsp.content with
  | "" -> Ok None
  | s ->
     (try Ok (Some (Yojson.Safe.from_string s))
      with
      | exn ->
         let error = match exn with
           | Yojson.Json_error e -> e
           | e -> Printexc.to_string e in
         make_error
           ~error
           ~response:s
           ~message:"Failed to parse response body"
           ())

let http_api_call ?timeout ?with_credentials
      ?(body : 'a Js.t option)
      ~meth
      url
    : (ok, error) Lwt_result.t =
  let content_type = match meth with
    | `POST -> Some ("Content-Type", "application/json")
    | _ -> None in
  let (headers : (string * string) list) =
    ["Accept", "application/json, text/plain, */*"]
    |> List.cons_maybe content_type in
  let contents = match body with
    | None -> None
    | Some b ->
       let s = Js.to_string @@ Json.output b in
       Some (`String s) in
  let t =
    Lwt_xmlHttpRequest.perform_raw
      ?with_credentials
      ?contents
      ~headers
      ~override_method:meth
      ~response_type:XmlHttpRequest.Text
      url
    >|= fun x ->
    match x.code with
    | 0 -> make_error ~message:"No response from the server" ()
    | 200 -> parse_response x
    | _ ->
       let response = Js.to_string x.content in
       make_error ~message:"API call failed" ~response () in
  Lwt.catch (fun () ->
      match timeout with
      | None -> t
      | Some (x : float) ->
         let sleep =
           Lwt_js.sleep (x /. 1000.)
           >|= (make_error ~timeout:x ~message:"Request timed out") in
         Lwt.choose [t; sleep])
    (fun exn -> Lwt.return @@ make_error ~message:(Printexc.to_string exn) ())
