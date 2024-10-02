open Yojson.Basic.Util

let () =
  try
    let _ = Check_info.check_file "./json/unary_sub.json" in
    ()
  with 
  | Type_error (msg, _) ->
      print_endline ("Error : " ^ msg)
  | Yojson__Common.Json_error msg ->
      print_endline ("Error : " ^ msg)
  | Sys_error msg ->
      print_endline ("Error : " ^ msg)