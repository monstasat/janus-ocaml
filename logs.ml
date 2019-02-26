include Lwt_log_js

let (section : Lwt_log_js.section) =
  Lwt_log_js.Section.make "janus-js"

let debug = debug ~section
let debug_f = debug_f ~section
let ign_debug = ign_debug ~section
let ign_debug_f = ign_debug_f ~section

let info = info ~section
let info_f = info_f ~section
let ign_info = ign_info ~section
let ign_info_f = ign_info_f ~section

let notice = notice ~section
let notice_f = notice_f ~section
let ign_notice = ign_notice ~section
let ign_notice_f = ign_notice_f ~section

let warning = warning ~section
let warning_f = warning_f ~section
let ign_warning = ign_warning ~section
let ign_warning_f = ign_warning_f ~section

let error = error ~section
let error_f = error_f ~section
let ign_error = ign_error ~section
let ign_error_f = ign_error_f ~section

let fatal = fatal ~section
let fatal_f = fatal_f ~section
let ign_fatal = ign_fatal ~section
let ign_fatal_f = ign_fatal_f ~section
