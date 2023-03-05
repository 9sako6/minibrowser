type selector =
  | Universal_selector
  | Class_selector of string

type size_unit = Px

type value =
  | Keyword of string
  | Size of float * size_unit

type declaration = Declaration of string * value
type rule = Rule of selector list * declaration list
type stylesheet = Stylesheet of rule list

let string_of_value = function
  | Keyword keyword -> keyword
  | Size (size, unit) -> (
      match unit with
      | Px -> Printf.sprintf "%s px" (string_of_float size))

let string_of_declaration = function
  | Declaration (name, value) ->
      Printf.sprintf "Declaration(%s: %s)" name (string_of_value value)

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
