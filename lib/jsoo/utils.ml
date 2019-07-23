open Js_of_ocaml

let ( % ) a b c = a (b c)

module List = struct
  include List

  let set_assoc ~eq k v l =
    let rec aux ~eq k v = function
      | acc, [] -> (k, v) :: List.rev acc
      | acc, (a, b) :: tl ->
        if eq a k
        then List.rev_append acc ((k, v) :: tl)
        else aux ~eq k v (((a, b) :: acc), tl) in
    aux ~eq k v ([], l)

  let cons_maybe (x : 'a option) (l : 'a list) : 'a list =
    match x with
    | None -> l
    | Some x -> x :: l

  let find_map (f : 'a -> 'b option) (l : 'a list) : 'b option =
    let rec aux = function
      | [] -> None
      | hd :: tl ->
        match f hd with
        | None -> aux tl
        | Some _ as res -> res in
    aux l

end

module Result = struct

  module Infix = struct

    let ( >>= ) r f = match r with
      | Error e -> Error e
      | Ok x -> f x

    let ( >|= ) r f = match r with
      | Error e -> Error e
      | Ok x -> Ok (f x)

  end

end

module Option = struct

  let to_string (f : 'a -> string) : 'a option -> string = function
    | None -> "None"
    | Some x -> f x

  let equal ~(eq : 'a -> 'a -> bool) (a : 'a option) (b : 'a option) : bool =
    match a, b with
    | None, None | None, Some _ | Some _, None -> false
    | Some a, Some b -> eq a b

  let is_some : 'a option -> bool = function
    | None -> false
    | Some _ -> true

  let is_none : 'a option -> bool = function
    | None -> true
    | Some _ -> false

  let map (f : 'a -> 'b) (x : 'a option) : 'b option =
    match x with
    | None -> None
    | Some x -> Some (f x)

  let flat_map (f : 'a -> 'b option) (x : 'a option) : 'b option =
    match x with
    | None -> None
    | Some x -> f x

  let iter (f : 'a -> unit) (x : 'a option) : unit =
    match x with
    | None -> ()
    | Some x -> f x

  let to_result (e : 'e) : 'a option -> ('a, 'e) result = function
    | Some x -> Ok x
    | None -> Error e

  let to_result_lazy (f : unit -> 'e) : 'a option -> ('a, 'e) result = function
    | Some x -> Ok x
    | None -> Error (f ())

end

module String : sig
  include module type of String
  val random : int -> string
end = struct
  include String

  let char_set = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

  let random (length : int) : string =
    let len = String.length char_set in
    String.init length (fun _ ->
        let poz =
          float_of_int len *. Random.float 1.
          |> Float.floor
          |> int_of_float in
        String.get char_set poz)

end

let add_query (uri : string)
      (query : (string * string list) list) : string =
  match query with
  | [] -> uri
  | q ->
     List.map (function
         | k, [] -> k
         | k, [v] -> Printf.sprintf "%s=%s" k v
         | k, v -> Printf.sprintf "%s=%s" k @@ String.concat "," v) q
     |> String.concat "&"
     |> Printf.sprintf "%s?%s" uri

let cast_list (x : 'a Js.t) : 'a Js.t list option =
  let array_constr : 'a Js.js_array Js.t Js.constr =
    Js.Unsafe.global##._Array in
  if Js.instanceof x array_constr
  then Some (Array.to_list @@ Js.to_array (Js.Unsafe.coerce x))
  else None

let default_ice () =
  let url = "stun:stun.l.google.com:19302" in
  let (o : Webrtc._RTCIceServer Js.t) = Js.Unsafe.obj [||] in
  o##.urls := Js.string url;
  [o]

let notify_error_lwt (t : string Lwt.t) = function
  | None -> ()
  | Some f ->
     Lwt.(
      ignore_result
      @@ catch (fun () -> t >|= fun s -> f s)
           (function
            | Canceled -> return_unit
            | exn -> return @@ f @@ Printexc.to_string exn))

let array_get a i =
  Js.Optdef.to_option @@ Js.array_get a i

let exn_to_string : exn -> string = function
  | Js.Error e -> Js.to_string e##toString
  | Failure s -> s
  | exn -> Printexc.to_string exn

let is_webrtc_supported () : bool =
  let test_def = Js.Optdef.test in
  let test_opt = Js.Opt.test in
  let coerce = Js.Unsafe.coerce in
  let wnd = Dom_html.window in
  test_def (coerce wnd)##.RTCPeerConnection
  && test_opt (coerce wnd)##.RTCPeerConnection

let is_get_user_media_available () : bool =
  let test_def = Js.Optdef.test in
  let test_opt = Js.Opt.test in
  let coerce = Js.Unsafe.coerce in
  let nav = coerce Dom_html.window##.navigator in
  test_def (coerce nav)##.mediaDevices
  && test_opt (coerce nav)##.mediaDevices
  && test_def (coerce nav)##.mediaDevices##.getUserMedia
  && test_opt (coerce nav)##.mediaDevices##.getUserMedia

let is_webrtc_supported_lwt () : (unit, string) Lwt_result.t =
  if is_webrtc_supported () then Lwt_result.return () else (
    let s = "WebRTC is not supported by this browser" in
    Log.ign_error s;
    Lwt_result.fail s)

let is_get_user_media_available_lwt () : (unit, string) Lwt_result.t =
  if is_get_user_media_available () then Lwt_result.return () else (
    let s = "navigator.mediaDevices is unavailable" in
    Log.ign_error s;
    Lwt_result.fail s)

let is_connected_lwt (f : unit -> bool) : (unit, string) Lwt_result.t =
  if f () then Lwt_result.return () else (
    let s = "Is the server down? (connected=false)" in
    Log.ign_warning s;
    Lwt_result.fail s)
