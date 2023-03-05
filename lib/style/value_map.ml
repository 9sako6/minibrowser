module Value_map = Map.Make (String)

type t = Css.Node.value Value_map.t

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
        let string =
          Printf.sprintf "%s: %s;" name (Css.Node.string_of_value value)
        in
        acc (strings @ [ string ]) rest
  in
  let strings, _ = acc [] bindings in
  String.concat " " strings

let rec lookup keys default_value map =
  match keys with
  | [] -> default_value
  | key :: rest -> (
      try find key map with Not_found -> lookup rest default_value map)

let%expect_test "lookup" =
  let open Css.Node in
  let value = Size (12., Px) in
  empty |> add "padding" value
  |> lookup [ "padding-left"; "padding" ] (Keyword "default")
  |> string_of_value |> print_endline;
  [%expect {| 12. px |}]

let%expect_test "lookup" =
  let open Css.Node in
  empty
  |> lookup [ "padding-left"; "padding" ] (Keyword "default")
  |> string_of_value |> print_endline;
  [%expect {| default |}]
