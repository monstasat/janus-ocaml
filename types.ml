open Js_of_ocaml

type log_level =
  | Error
  | Warn
  | Info
  | Log
  | Debug

type log =
  { prn : 'a. 'a -> unit
  ; prn_2 : 'a 'b. 'a -> 'b -> unit

  ; str : string -> unit
  ; str_2 : 'a. string -> 'a -> unit
  }

let log_level_to_int : log_level -> int = function
  | Error -> 0
  | Warn -> 1
  | Info -> 2
  | Log -> 3
  | Debug -> 4

let log_level_to_string : log_level -> string = function
  | Error -> "error"
  | Warn -> "warn"
  | Info -> "info"
  | Log -> "log"
  | Debug -> "debug"

let can_log (set : log_level option) (cur : log_level) : bool =
  match set with
  | None -> false
  | Some set -> not (log_level_to_int cur > log_level_to_int set)

type logs =
  { err : log
  ; warn : log
  ; info : log
  ; log : log
  ; debug : log
  }

let make_log ?(log_level : log_level option) (level : log_level) : log =
  if not (can_log log_level level)
  then
    { prn = (fun _ -> ())
    ; prn_2 = (fun _ _ -> ())
    ; str = (fun _ -> ())
    ; str_2 = (fun _ _ -> ())
    }
  else
    let meth arr =
      let s = log_level_to_string level in
      Js.Unsafe.(meth_call global##.console s arr) in
    { prn = (fun a -> meth [|Any._of a|])
    ; prn_2 = (fun a b -> meth [|Any._of a; Any._of b|])
    ; str = (fun a -> meth [|Any.of_string a|])
    ; str_2 = (fun a b -> meth [|Any.of_string a; Any._of b|])
    }

let make_logs ?(log_level : log_level option) () : logs =
  { err = make_log ?log_level Error
  ; warn = make_log ?log_level Warn
  ; info = make_log ?log_level Info
  ; log = make_log ?log_level Log
  ; debug = make_log ?log_level Debug
  }

type ice_transport_policy =
  | All
  | Public
  | Relay

let ice_transport_policy_to_string : ice_transport_policy -> string = function
  | All -> "all"
  | Public -> "public"
  | Relay -> "relay"

type bundle_policy =
  | Balanced
  | Max_compact
  | Max_bundle

let bundle_policy_to_string : bundle_policy -> string = function
  | Balanced -> "balanced"
  | Max_compact -> "max-compact"
  | Max_bundle -> "max-bundle"
