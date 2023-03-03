type selector =
  | Universal_selector
  | Type_selector of string
  | Class_selector of string

type declaration = Declaration of string * string
type rule = selector list * declaration list
type stylesheet = rule list

exception Invalid_declaration

let string_of_declaration = function
  | Declaration (key, value) -> Printf.sprintf "Declaration(%s: %s)" key value

let parse_declaration tokens =
  match tokens with
  | key :: ":" :: value :: ";" :: rest -> (Declaration (key, value), rest)
  | _ -> raise Invalid_declaration

let%expect_test "parse_declaration" =
  let declaration, rest =
    parse_declaration [ "margin"; ":"; "left"; ";"; "}"; "."; "foo"; "{"; "}" ]
  in
  print_endline (string_of_declaration declaration);
  assert (rest = [ "}"; "."; "foo"; "{"; "}" ]);
  [%expect {| Declaration(margin: left) |}]

let parse tokens = tokens
