open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Infix
open Utils

type 'a frame = 'a Lwt_xmlHttpRequest.generic_http_frame

type error_ext =
  { response : string option
  ; error : string option
  ; timeout : int option
  }
and error = string * error_ext option
and ok = Yojson.Safe.json option
and t = (ok, error) result

let make_error ?timeout ?error ?response ?message ()
    : ('a, error) result =
  let message = match message with
    | None | Some "" -> "Internal error"
    | Some s -> s in
  let ext = match timeout, error, response with
    | None, None, None -> None
    | _ -> Some { response; error; timeout } in
  Error (message, ext)

let error_to_string ((text, ext) : error) : string =
  let option_to_string k f v = match v with
    | None -> ""
    | Some r -> Printf.sprintf "%s = %s" k (f r) in
  match ext with
  | None -> Printf.sprintf "%s" text
  | Some { response; error; timeout } ->
     let details =
       String.concat ", "
         [ option_to_string "response" (fun x -> x) response
         ; option_to_string "error" (fun x -> x) error
         ; option_to_string "timeout" (Printf.sprintf "%d") timeout ] in
     Printf.sprintf "%s: \n%s" text details

let parse_response (rsp : Js.js_string Js.t frame) =
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

let http_api_call ?async ?timeout ?with_credentials
      ?(body : 'a Js.t option)
      ~meth
      (uri : Uri.t)
    : t Lwt.t =
  ignore async;
  let content_type = match meth with
    | `POST -> Some ("Content-Type", "application/json")
    | _ -> None in
  let (headers : (string * string) list) =
    ["Accept", "application/json, text/plain, */*"]
    |> List.cons_maybe content_type in
  let contents = match body with
    | None -> None
    | Some b -> Some (`String (Js.to_string @@ Json.output b)) in
  let t =
    Lwt_xmlHttpRequest.perform_raw
      ?with_credentials
      ?contents
      ~headers
      ~override_method:meth
      ~response_type:XmlHttpRequest.Text
      (Uri.to_string uri)
    >|= fun x ->
    match x.code with
    | 0 ->
       let s = "Probably a network error, is the server down?" in
       make_error ~message:s ()
    | 200 -> parse_response x
    | _ ->
       let response = Js.to_string x.content in
       make_error ~message:"API call failed" ~response () in
  Lwt.catch (fun () ->
      match timeout with
      | None -> t
      | Some (x : int) ->
         let sleep =
           Lwt_js.sleep @@ (float_of_int x /. 1000.)
           >|= (make_error ~timeout:x ~message:"Request timed out") in
         Lwt.choose [t; sleep])
    (fun exn -> Lwt.return @@ make_error ~message:(Printexc.to_string exn) ())
