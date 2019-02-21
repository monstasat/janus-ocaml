open Js_of_ocaml

module ICE = struct

  type t =
    { urls : string list
    ; username : string option
    ; credential : string option
    }

  let make ?username ?credential ?(urls = []) () : t =
    { urls; username; credential }

  let to_js (t : t) : 'a Js.t =
    let urls = match t.urls with
      | [] -> Any.of_jsobj @@ Jsobj.empty
      | [x] -> Any.of_string x
      | l -> Any.of_list Any.of_string l in
    Jsobj.make_js ["urls", urls]

  let default : t list =
    [make ~urls:["stun:stun.l.google.com:19302"] ()]

end
