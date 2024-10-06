open Yojson.Basic.Util

exception No_way of string

let is_final json status =

  let check_final status final =
    if status = final then
      true
    else
      false
  in

  let finals = json |> member "finals" |> to_list |> List.map to_string in
  if List.exists (check_final status) finals = true then
    true
  else
    false

let print_info json =
  
  let print_list alphabet tp =
    let rec position_in_lst lst target index =
      match lst with
      | [] -> -1
      | x :: xs -> if x = target then index else position_in_lst xs target (index + 1)
    in
    let pos = position_in_lst alphabet tp 0 in
    if pos + 1 = List.length alphabet then begin
      Printf.printf "%s " tp;
      ()
    end else begin
      Printf.printf "%s, " tp;
      ()
    end
  in

  let print_name name =
    let print_name_line name car =
      let rec interval_print name car index =
        if index < String.length name then begin
          Printf.printf "%c" car;
          interval_print name car (index + 1);
          ()
        end else begin
          ()
        end
      in
      if car = '*' then begin
        Printf.printf "********************************";
        interval_print name '*' 0;
        Printf.printf "********************************\n";
        ()
      end else if car = 'n' then begin
        Printf.printf "*                               ";
        Printf.printf "%s" name;
        Printf.printf "                               *\n";
        ()
      end else if car = ' ' then begin
        Printf.printf "*                               ";
        interval_print name ' ' 0;
        Printf.printf "                               *\n";
        ()
      end else begin
        ()
      end
    in
    print_name_line name '*';
    print_name_line name ' ';
    print_name_line name 'n';
    print_name_line name ' ';
    print_name_line name '*';
    ()
  in

  let print_alphabet alphabet =
    Printf.printf "Alphabet : [ ";
    List.iter (print_list alphabet) alphabet;
    Printf.printf "]\n";
    ()
  in

  let print_blank blank =
    Printf.printf "Blank : '%s'\n" blank;
    ()
  in

  let print_states states =
    Printf.printf "States : [ ";
    List.iter (print_list states) states;
    Printf.printf "]\n";
    ()
  in

  let print_initial initial =
    Printf.printf "Initial : %s\n" initial;
    ()
  in

  let print_finals finals =
    Printf.printf "Finals : [ ";
    List.iter (print_list finals) finals;
    Printf.printf "]\n";
    ()
  in

  let print_transitions transitions =
    let print_transitions_info (name, lst_c) =
      let print_transitions_format name lst =
        let read = lst |> member "read" |> to_string in
        let to_state = lst |> member "to_state" |> to_string in
        let write = lst |> member "write" |> to_string in
        let action = lst |> member "action" |> to_string in
        Printf.printf "(%s, %s) -> (%s, %s, %s)\n" name read to_state write action;
        ()
      in
      let lst = lst_c |> to_list in
      List.iter (print_transitions_format name) lst;
      ()
    in
    List.iter print_transitions_info transitions;
    ()
  in

  let name = json |> member "name" |> to_string in
  let alphabet = json |> member "alphabet" |> to_list |> List.map to_string in
  let blank = json |> member "blank" |> to_string in
  let states = json |> member "states" |> to_list |> List.map to_string in
  let initial = json |> member "initial" |> to_string in
  let finals = json |> member "finals" |> to_list |> List.map to_string in
  let transitions = json |> member "transitions" |> to_assoc in

  print_name name;
  print_alphabet alphabet;
  print_blank blank;
  print_states states;
  print_initial initial;
  print_finals finals;
  print_transitions transitions;
  print_endline "----------------------------------------------------------------";
  ()

let launch json tape =
  let rec add_blank blank tape =
    let len = String.length tape in
    if len < 20 then
      let tape = tape ^ blank in
      add_blank blank tape
    else
      tape
  in

  let print_status json tape index status =

    let print_tape tape index =

      let rec print_char tape index lim =
        if index < lim then  begin
          let car = String.get tape index in
          Printf.printf "%c" car;
          print_char tape (index + 1) lim;
          ()
        end else begin
          ()
        end
      in

      let car = String.get tape index in
      let len = String.length tape in
      Printf.printf "[";
      print_char tape 0 index;
      Printf.printf "<%c>" car;
      print_char tape (index + 1) len;
      Printf.printf "] ";
      ()
    in

    let print_trans status car lst =

      let print_trans_found car trans =

        let read_car = trans |> member "read" |> to_string in
        if car = read_car then
          true
        else
          false
      in

      let print_trans_aff state car trans =
        let read_car = trans |> member "read" |> to_string in
        if car = read_car then begin
          let to_state = trans |> member "to_state" |> to_string in
          let write = trans |> member "write" |> to_string in
          let action = trans |> member "action" |> to_string in
          Printf.printf "(%s, %s) -> (%s, %s, %s)\n" state car to_state write action;
        end else begin
          ()
        end
      in

      if List.exists (print_trans_found car) lst = true then begin
        List.iter (print_trans_aff status car) lst
      end else begin
        raise (No_way ("Machine stuck, state : '" ^ status ^ "', '" ^ car ^ "'."))
      end
    in

    let final = is_final json status in

    if final = false then
      let transitions = json |> member "transitions" |> to_assoc in 
      let next = List.assoc status transitions |> to_list in
      let car_c = String.get tape index in
      let car = String.make 1 car_c in
      print_tape tape index;
      print_trans status car next;
      ()
    else
      ()
  in

  let rec change_state json tape index status =

    let get_line json tape index status =

      let rec get_line_check state car =
        match state with
        | [] -> raise (No_way ("Machine stuck, state : '" ^ status ^ "'."))
        | x :: xs -> let var = x |> member "read" |> to_string in if car = var then x else get_line_check xs car
      in

      let trans = json |> member "transitions" |> to_assoc in
      let state = List.assoc status trans |> to_list in
      let car_c = String.get tape index in
      let car = String.make 1 car_c in
      let line = get_line_check state car in
      line
    in

    let get_new_tape tape line cursor = 

      let rec write_tape tape new_tape index lim =
        if index < lim then
          let car_c = String.get tape index in
          let car = String.make 1 car_c in
          let new_tape = car ^ write_tape tape new_tape (index + 1) lim in
          new_tape
        else
          ""
      in

      let new_car = line |> member "write" |> to_string in
      let tape_len = String.length tape in
      let new_tape = write_tape tape "" 0 cursor in
      let new_tape = new_tape ^ new_car in
      let new_tape = new_tape ^ write_tape tape "" (cursor + 1) tape_len in
      new_tape
    in

    let get_new_state line =
      let new_state = line |> member "to_state" |> to_string in
      new_state
    in

    let get_new_index line index =
      let dir = line |> member "action" |> to_string in
      if dir = "RIGHT" then
        index + 1
      else
        index - 1
    in

    let adjust_tape json tape index =
      let blank = json |> member "blank" |> to_string in
      let len = String.length tape in
      if index >= len then
        tape ^ blank
      else if index < 0 then
        blank ^ tape
      else
        tape
    in

    let adjust_index index =
      if index < 0 then
        0
      else
        index
    in

    let print_final_tape tape =
      print_endline "----------------------------------------------------------------";
      Printf.printf "Tape final state : [%s]\n" tape;
    in

    let final = is_final json status in

    if final = false then
      let line = get_line json tape index status in
      let new_tape = get_new_tape tape line index in
      let new_status = get_new_state line in
      let new_index = get_new_index line index in
      let new_tape = adjust_tape json new_tape new_index in
      let new_index = adjust_index new_index in
      print_status json new_tape new_index new_status;
      change_state json new_tape new_index new_status;
      ()
    else
      print_final_tape tape;
      ()
  in

  let index = 0 in
  let status = json |> member "initial" |> to_string in
  let blank = json |> member "blank" |> to_string in
  let n_tape = add_blank blank tape in
  print_status json n_tape index status;
  change_state json n_tape index status;
  ()