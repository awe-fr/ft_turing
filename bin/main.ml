open Yojson.Basic.Util

let () =
  let input = "111-11=" in
  let path = "./json/unary_sub.json" in
  try
    let json = Check_info.check_file path input in
    Turing_machine.print_info json;
    ()
  with 
  | Type_error (msg, _) ->
      print_endline ("Error : " ^ msg)
  | Yojson__Common.Json_error msg ->
      print_endline ("Error : " ^ msg)
  | Sys_error msg ->
      print_endline ("Error : " ^ msg)
  | Check_info.Wrong_content msg ->
      print_endline ("Error : " ^ msg)
  | _ ->
      print_endline ("Unknown error")