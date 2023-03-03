type selector =
  | Universal_selector of string
  | Class_selector of string

type declaration = Declaration of string * string
type rule = Rule of selector list * declaration list
type stylesheet = Stylesheet of rule list

exception Invalid_declaration
exception Unknown_selector

let string_of_declaration = function
  | Declaration (name, value) -> Printf.sprintf "Declaration(%s: %s)" name value

let string_of_selector = function
  | Universal_selector name -> Printf.sprintf "Universal_selector(%s)" name
  | Class_selector name -> Printf.sprintf "Class_selector(%s)" name

let string_of_rule = function
  | Rule (selectors, declarations) ->
      let selectors_string =
        selectors |> List.map string_of_selector |> String.concat "; "
      in
      let declarations_string =
        declarations |> List.map string_of_declaration |> String.concat "; "
      in
      Printf.sprintf "Rule([%s], [%s])" selectors_string declarations_string

let string_of_stylesheet stylesheet =
  match stylesheet with
  | Stylesheet rules ->
      let rules_string =
        rules |> List.map string_of_rule |> String.concat "; "
      in
      Printf.sprintf "Stylesheet([%s])" rules_string

let rec parse_declaration tokens =
  match tokens with
  | "{" :: rest -> parse_declaration rest
  | name :: ":" :: value :: ";" :: rest -> (Declaration (name, value), rest)
  | _ -> raise Invalid_declaration

let%expect_test "parse_declaration" =
  let declaration, rest =
    parse_declaration [ "margin"; ":"; "left"; ";"; "}"; "."; "foo"; "{"; "}" ]
  in
  print_endline (string_of_declaration declaration);
  assert (rest = [ "}"; "."; "foo"; "{"; "}" ]);
  [%expect {| Declaration(margin: left) |}]

let parse_declarations tokens =
  let rec acc declarations rest =
    match rest with
    | "}" :: _ -> (declarations, rest)
    | _ ->
        let declaration, rest = parse_declaration rest in
        acc (declarations @ [ declaration ]) rest
  in
  acc [] tokens

let parse_selector tokens =
  let rec split selector_tokens rest =
    match rest with
    | [] -> (selector_tokens, [])
    | "{" :: _ -> (selector_tokens, rest)
    | "," :: rest -> (selector_tokens, rest)
    | head :: rest -> split (selector_tokens @ [ head ]) rest
  in
  let selector_tokens, rest = split [] tokens in
  let selector =
    match selector_tokens with
    | "*" :: t -> Universal_selector (String.concat "" t)
    | "." :: t -> Class_selector (String.concat "" t)
    | _ -> raise Unknown_selector
  in
  (selector, rest)

let parse_comma_separated_selectors tokens =
  let rec acc selectors rest =
    match rest with
    | "{" :: _ -> (selectors, rest)
    | _ ->
        let selector, rest = parse_selector rest in
        acc (selectors @ [ selector ]) rest
  in
  let selectors, rest = acc [] tokens in
  (selectors, rest)

let parse_rule tokens =
  let rec split rule_tokens rest =
    match rest with
    | [] -> (rule_tokens, [])
    | "}" :: rest -> (rule_tokens @ [ "}" ], rest)
    | head :: rest -> split (rule_tokens @ [ head ]) rest
  in
  let rule_tokens, rule_rest = split [] tokens in
  let selectors, rest = parse_comma_separated_selectors rule_tokens in
  let declarations, _ = parse_declarations rest in
  (Rule (selectors, declarations), rule_rest)

let parse_rules tokens =
  let rec acc rules rest =
    match rest with
    | [] -> (rules, [])
    | _ ->
        let rule, rest = parse_rule rest in
        acc (rules @ [ rule ]) rest
  in
  acc [] tokens

let parse tokens =
  let rules, _ = parse_rules tokens in
  Stylesheet rules

let%expect_test "parse" =
  "\n.foo,.bar {\n  display: flex;\n  color: red;\n}\n" |> Tokenizer.tokenize
  |> parse |> string_of_stylesheet |> print_endline;
  [%expect
    {| Stylesheet([Rule([Class_selector(foo); Class_selector(bar)], [Declaration(display: flex); Declaration(color: red)])]) |}]
