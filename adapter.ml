open Js_of_ocaml

class type browserDetails =
  object
    method version : int Js.readonly_prop
    method browser : Js.js_string Js.t Js.readonly_prop
  end

let browser_details : browserDetails Js.t =
  Js.Unsafe.global##.adapter##.browserDetails

let get_browser () : string =
  Js.to_string browser_details##.browser

let check_browser ?(browser : string option)
      ?(ver_cmp = ( = ))
      ?(ver : int option)
      () : bool =
  match browser, ver with
  | None, None -> true
  | None, Some version -> ver_cmp browser_details##.version version
  | Some browser', None ->
     String.equal browser' (get_browser ())
  | Some browser', Some version ->
     String.equal browser' (get_browser ())
     && ver_cmp browser_details##.version version
