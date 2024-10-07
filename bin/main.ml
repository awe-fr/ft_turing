open Yojson.Basic.Util



let () =

  let help () = 
    Printf.printf "usage: ft_turing [-h] jsonfile input\n\n";
    Printf.printf "positional arguments:\n";
    Printf.printf "    jsonfile        json description of the machine\n\n";
    Printf.printf "    input        input of the machine\n\n";
    Printf.printf "optional arguments:\n";
    Printf.printf "    -h, --help        show this help message and exit\n";
    ()
  in

  let rec check_help ac index =
    if index >= ac then
      -1
    else
      let cmd = Sys.argv.(index) in
      if cmd = "--help" || cmd = "-h" then begin
        help ();
        1
      end else begin
        check_help ac (index + 1)
      end
  in

  let ac = Array.length Sys.argv in
  let is = check_help ac 1 in
  if is = -1 then
    if ac = 3 then begin
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
    end else begin
      Printf.printf "Error : Wrong arguments : ./ft_turing -h\n";
      ()
    end
  else
    ()