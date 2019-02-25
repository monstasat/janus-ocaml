open Js_of_ocaml

module List = struct
  include List

  let rec mem ~(eq : 'a -> 'a -> bool) (x : 'a) = function
    | [] -> false
    | hd :: tl -> if eq hd x then true else mem ~eq x tl

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
end

module Option = struct

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

module Uri = struct
  include Uri

  let append_path (uri : t) (s : string) =
    let path = path uri in
    with_path uri (Printf.sprintf "%s/%s" path s)
end

let ( % ) a b c = a (b c)

let array_get a i =
  Js.Optdef.to_option @@ Js.array_get a i

let to_js_string_array =
  Js.array % Array.of_list % List.map Js.string

let is_webrtc_supported () : bool =
  let test_def = Js.Optdef.test in
  let test_opt = Js.Opt.test in
  let coerce = Js.Unsafe.coerce in
  let wnd = Dom_html.window in
  let nav = wnd##.navigator in
  test_def (coerce wnd)##.RTCPeerConnection
  && test_opt (coerce wnd)##.RTCPeerConnection
  && test_def (coerce nav)##.getUserMedia
  && test_opt (coerce nav)##.getUserMedia
