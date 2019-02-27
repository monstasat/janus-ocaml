include Lwt_log_js

let logger = Lwt_log_js.console

let (section : Lwt_log_js.section) =
  Lwt_log_js.Section.make "janus"

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
