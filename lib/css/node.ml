type selector =
  | Universal_selector
  | Class_selector of string

type declaration = Declaration of string * Value.t
type rule = Rule of selector list * declaration list
type stylesheet = Stylesheet of rule list

let string_of_declaration = function
  | Declaration (name, value) ->
      Printf.sprintf "Declaration(%s: %s)" name (Value.show value)

let string_of_selector = function
  | Universal_selector -> Printf.sprintf "Universal_selector"
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
