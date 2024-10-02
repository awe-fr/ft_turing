open Yojson.Basic
open Yojson.Basic.Util

exception Wrong_format of string

let check_file path =
  try
    let json = from_file path in
    try
      let name = json |> member "name" |> to_string in
      print_endline name;
      json
    with _ ->
      raise (Wrong_format "Jsons content")
  with _ ->
    raise (Wrong_format "Jsons format")