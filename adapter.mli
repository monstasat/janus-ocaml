type browser_details =
  { version : int
  ; browser : string
  }

type t =
  { browser_details : browser_details
  }

val adapter : t [@@js.global "adapter"]
