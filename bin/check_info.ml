open Yojson.Basic
open Yojson.Basic.Util

exception Wrong_content of string

let check_file path input =
  let check_individual individual =
    let _ = individual |> member "read" |> to_string in
    let _ = individual |> member "to_state" |> to_string in
    let _ = individual |> member "write" |> to_string in
    let _ = individual |> member "action" |> to_string in
    ()
  in
  
  let check_group (_,group_list) =
    let individuals = group_list |> to_list in
    List.iter check_individual individuals
  in

  let check_name name =
    if String.length name <> 0 then
      ()
    else
      raise (Wrong_content "Bad name.")
  in

  let check_alphabet letter =
    if String.length letter = 1 then
      ()
    else
      raise (Wrong_content "Bad alphabet.")
  in

  let check_blank lst blank = 
    if List.mem blank lst && String.length blank > 0 then
      ()
    else
      raise (Wrong_content "Blank not in alphabet.")
  in

  let check_states states =
    if String.length states <> 0 then
      ()
    else
      raise (Wrong_content "Bad states.")
  in

  let check_initial lst initial = 
    if List.mem initial lst then
      ()
    else
      raise (Wrong_content "Initial not a valid state.")
  in

  let check_finals lst final = 
    if List.mem final lst then
      ()
    else
      raise (Wrong_content "Finals not a valid state.")
  in

  let check_transition_one states finals (lst_name, lst_content) =
    let content = lst_content |> to_list in
    if content <> [] then
      if List.mem lst_name states then
        if List.mem lst_name finals then
          raise (Wrong_content (lst_name ^ " : is also a final."))
        else
          ()
        else
        raise (Wrong_content (lst_name ^ " : Not a state."))
    else
      raise (Wrong_content (lst_name ^ " : Too short."))
  in

  let check_transition_two initial transitions =
    let same key (k,_) =
      if k = key then
        true
      else
        false
    in
    if List.exists (same initial) transitions = true then
      ()
    else
      raise (Wrong_content "Initial is not a part of transitions.")
  in

  let check_transition_three states finals transitions =
    let s = List.length states in
    let f = List.length finals in
    let t = List.length transitions in
    if f + t = s then
      ()
    else
      raise (Wrong_content "Some states are not used.")
  in

  let check_transition_four alphabet states (_, lst_n) =
    let check_lines alphabet states line =
      let read = line |> member "read" |> to_string in
      let to_state = line |> member "to_state" |> to_string in
      let write = line |> member "write" |> to_string in
      let action = line |> member "action" |> to_string in
      if List.mem read alphabet then
        if List.mem to_state states then
          if List.mem write alphabet then
            if action = "RIGHT" || action = "LEFT" then
              ()
            else
              raise (Wrong_content "Action content has to be RIGHT or LEFT.")
          else
            raise (Wrong_content "Write content is not in alphabet.")
        else
          raise (Wrong_content "To_state content is not a state.")
      else
        raise (Wrong_content "Read content is not in alphabet.")
    in
    let lst = lst_n |> to_list in
    List.iter (check_lines alphabet states) lst
  in
  
  let rec check_input alphabet blank input index =
    if String.length input <= 0 then
      raise (Wrong_content "Input too short.")
    else
      if index >= String.length input then
        ()
      else
        let car_c = String.get input index in
        let car = String.make 1 car_c in
        if List.mem car alphabet then
          if car <> blank then
            check_input alphabet blank input (index + 1)
          else
            raise (Wrong_content ("Input '" ^ car ^ "' : is the blank character."))
        else
          raise (Wrong_content ("Input '" ^ car ^ "' : is not in the alphabet."))
  in

  try
    let json = from_file path in
    let name = json |> member "name" |> to_string in
    let alphabet = json |> member "alphabet" |> to_list |> List.map to_string in
    let blank = json |> member "blank" |> to_string in
    let states = json |> member "states" |> to_list |> List.map to_string in
    let initial = json |> member "initial" |> to_string in
    let finals = json |> member "finals" |> to_list |> List.map to_string in
    let transitions = json |> member "transitions" |> to_assoc in
    List.iter check_group transitions;

    check_name name;
    List.iter check_alphabet alphabet;
    check_blank alphabet blank;
    List.iter check_states states;
    check_initial states initial;
    List.iter (check_finals states) finals;
    List.iter (check_transition_one states finals) transitions;
    check_transition_two initial transitions;
    check_transition_three states finals transitions;
    List.iter (check_transition_four alphabet states) transitions;

    check_input alphabet blank input 0;

    json
  with 
  | Type_error (msg, v) ->
      raise (Type_error ("Failed to find all requierment. " ^ msg, v))
  | Yojson__Common.Json_error msg ->
      raise (Yojson__Common.Json_error ("Bad format. " ^ msg))
  | Sys_error msg ->
      raise (Sys_error msg)
  | Wrong_content msg ->
      raise (Wrong_content msg)