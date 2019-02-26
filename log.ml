include Lwt_log_js
open Js_of_ocaml

module Console = struct

  class type t =
    object
      method log : 'a. 'a -> unit Js.meth
      method log_2 : 'a 'b. 'a -> 'b -> unit Js.meth

      method debug : 'a. 'a -> unit Js.meth
      method debug_2 : 'a 'b. 'a -> 'b -> unit Js.meth

      method info : 'a. 'a -> unit Js.meth
      method info_2 : 'a 'b. 'a -> 'b -> unit Js.meth

      method warn : 'a. 'a -> unit Js.meth
      method warn_2 : 'a 'b. 'a -> 'b -> unit Js.meth

      method error : 'a. 'a -> unit Js.meth
      method error_2 : 'a 'b. 'a -> 'b -> unit Js.meth

    end

  let (console : t Js.t) = Js.Unsafe.global##.console

end

let (logger : logger) =
  let js_val = Lwt.new_key () in
  make
    ~close:(fun _ -> Lwt.return_unit)
    ~output:(fun section level logs ->
      let str =
        Js.string
          (Printf.sprintf "[%s] %s"
             (Section.name section)
             (String.concat "\n" logs))
      in
      (match level, Lwt.get js_val with
       | Debug, None -> Console.console##debug str
       | Debug, Some v -> Console.console##debug_2 str v
       | Info, None | Notice, None-> Console.console##info str
       | Info, Some v | Notice, Some v -> Console.console##info_2 str v
       | Warning, None -> Console.console##warn str
       | Warning, Some v -> Console.console##warn_2 str v
       | Error, None | Fatal, None -> Console.console##error str
       | Error, Some v | Fatal, Some v -> Console.console##error_2 str v);
      Lwt.return_unit)

let (section : Lwt_log_js.section) =
  Lwt_log_js.Section.make "janus-js"

let debug = debug ~logger ~section
let debug_f = debug_f ~logger ~section
let ign_debug = ign_debug ~logger ~section
let ign_debug_f = ign_debug_f ~logger ~section

let info = info ~logger ~section
let info_f = info_f ~logger ~section
let ign_info = ign_info ~logger ~section
let ign_info_f = ign_info_f ~logger ~section

let notice = notice ~logger ~section
let notice_f = notice_f ~logger ~section
let ign_notice = ign_notice ~logger ~section
let ign_notice_f = ign_notice_f ~logger ~section

let warning = warning ~logger ~section
let warning_f = warning_f ~logger ~section
let ign_warning = ign_warning ~logger ~section
let ign_warning_f = ign_warning_f ~logger ~section

let error = error ~logger ~section
let error_f = error_f ~logger ~section
let ign_error = ign_error ~logger ~section
let ign_error_f = ign_error_f ~logger ~section

let fatal = fatal ~logger ~section
let fatal_f = fatal_f ~logger ~section
let ign_fatal = ign_fatal ~logger ~section
let ign_fatal_f = ign_fatal_f ~logger ~section
