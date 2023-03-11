module Value_map = Map.Make (String)
include Value_map

type t = Value.t Value_map.t

let pp map =
  let bindings = Value_map.bindings map in
  let rec aux bindings acc =
    match bindings with
    | [] -> (acc, [])
    | (name, value) :: rest ->
        let string = Printf.sprintf "%s: %s;" name (Value.show value) in
        aux rest (acc @ [ string ])
  in
  let strings, _ = aux bindings [] in
  String.concat " " strings

let rec lookup keys default_value map =
  match keys with
  | [] -> default_value
  | key :: rest -> (
      try find key map with Not_found -> lookup rest default_value map)
