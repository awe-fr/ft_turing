open Yojson.Basic.Util

exception No_way of string

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

    let transitions = json |> member "transitions" |> to_assoc in 
    let next = List.assoc status transitions |> to_list in
    let car_c = String.get tape index in
    let car = String.make 1 car_c in
    print_tape tape index;
    print_trans status car next;
  in

  let index = 0 in
  let status = json |> member "initial" |> to_string in
  let blank = json |> member "blank" |> to_string in
  let n_tape = add_blank blank tape in
  print_status json n_tape index status;
  ()