open Js_of_ocaml

type browser_details =
  { version : int
  ; browser : string
  }

let browser_details () : browser_details =
  let details = Js.Unsafe.global##.adapter##.browserDetails in
  { version = details##.version
  ; browser = Js.to_string details##.browser
  }
