open Yojson.Basic
open Yojson.Basic.Util

exception Wrong_content of string

let check_file path =
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
      raise (Wrong_content "Bad name")
  in

  let check_alphabet letter =
    if String.length letter = 1 then
      ()
    else
      raise (Wrong_content "Bad alphabet")
  in

  let check_blank lst blank = 
    if List.mem blank lst then
      ()
    else
      raise (Wrong_content "Blank not in alphabet")
  in

  let check_states states =
    if String.length states <> 0 then
      ()
    else
      raise (Wrong_content "Bad states")
  in

  let check_initial lst initial = 
    if List.mem initial lst then
      ()
    else
      raise (Wrong_content "Initial not a valid state")
  in

  let check_finals lst final = 
    if List.mem final lst then
      ()
    else
      raise (Wrong_content "Finals not a valid state")
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