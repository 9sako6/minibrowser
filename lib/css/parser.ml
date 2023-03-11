open Node

exception Invalid_declaration
exception Unknown_selector

let parse_declaration tokens =
  match tokens with
  | name :: ":" :: rest ->
      let rec parse_declaration_value value_tokens rest =
        match rest with
        | ";" :: rest -> (value_tokens, rest)
        | head :: rest -> parse_declaration_value (value_tokens @ [ head ]) rest
        | [] -> (value_tokens, [])
      in
      let value_tokens, rest = parse_declaration_value [] rest in
      (Declaration (name, Value.build value_tokens), rest)
  | _ -> raise Invalid_declaration

let parse_declarations tokens =
  let rec aux declarations rest =
    match rest with
    | "{" :: rest -> aux declarations rest
    | "}" :: _ -> (declarations, rest)
    | _ ->
        let declaration, rest = parse_declaration rest in
        aux (declarations @ [ declaration ]) rest
  in
  aux [] tokens

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
    | "*" :: _ -> Universal_selector
    | "." :: t -> Class_selector (String.concat "" t)
    | _ -> raise Unknown_selector
  in
  (selector, rest)

let parse_comma_separated_selectors tokens =
  let rec aux selectors rest =
    match rest with
    | "{" :: _ -> (selectors, rest)
    | _ ->
        let selector, rest = parse_selector rest in
        aux (selectors @ [ selector ]) rest
  in
  let selectors, rest = aux [] tokens in
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
  let rec aux rules rest =
    match rest with
    | [] -> (rules, [])
    | _ ->
        let rule, rest = parse_rule rest in
        aux (rules @ [ rule ]) rest
  in
  aux [] tokens

let parse tokens =
  let rules, _ = parse_rules tokens in
  Stylesheet rules
