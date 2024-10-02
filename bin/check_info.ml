open Yojson.Basic
open Yojson.Basic.Util

let check_file path =
  try
    let json = from_file path in
    let name = json |> member "name" |> to_string in
    let alphabet = json |> member "alphabet" |> to_list |> List.map to_string in
    let blank = json |> member "blank" |> to_string in
    let states = json |> member "states" |> to_list |> List.map to_string in
    let initial = json |> member "initial" |> to_string in
    let finals = json |> member "finals" |> to_list |> List.map to_string in
    (* let _ = json |> member "transitions" |>  *)

    print_endline name;
    List.iter print_endline alphabet;
    print_endline blank;
    List.iter print_endline states;
    print_endline initial;
    List.iter print_endline finals;
    json
  with 
  | Type_error (msg, v) ->
      raise (Type_error ("Failed to find all requierment. " ^ msg, v))
  | Yojson__Common.Json_error msg ->
      raise (Yojson__Common.Json_error ("Bad format. " ^ msg))
  | Sys_error msg ->
      raise (Sys_error msg)