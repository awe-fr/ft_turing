open Yojson.Basic.Util

let () =
  let ac = Array.length Sys.argv in
  if ac = 3 then
    try
        let json = Check_info.check_file Sys.argv.(1) Sys.argv.(2) in
        Turing_machine.print_info json;
        Turing_machine.launch json Sys.argv.(2);
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
    | Turing_machine.No_way msg ->
        print_endline ("Error : " ^ msg)
    | _ ->
        print_endline ("Unknown error")
  else
    print_endline "Wrong arguments : ./exec ./json tape";
    ()