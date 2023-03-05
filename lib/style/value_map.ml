module Value_map = Map.Make (String)

type t = string Value_map.t

let empty : t = Value_map.empty
let find key (map : t) = Value_map.find key map
let bindings (map : t) = Value_map.bindings map
let add key value (map : t) = Value_map.add key value map

let to_string map =
  let bindings = Value_map.bindings map in
  let rec acc strings bindings =
    match bindings with
    | [] -> (strings, [])
    | (name, value) :: rest ->
        let string = Printf.sprintf "%s: %s;" name value in
        acc (strings @ [ string ]) rest
  in
  let strings, _ = acc [] bindings in
  String.concat " " strings
