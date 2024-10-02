let () =
  try
    let _ = Check_info.check_file "./json/unary_sub.json" in
    ()
  with Check_info.Wrong_format msg ->
    print_endline ("Error : " ^ msg)