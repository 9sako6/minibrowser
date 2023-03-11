type selector =
  | Universal_selector
  | Class_selector of string

type declaration = Declaration of string * Value.t
type rule = Rule of selector list * declaration list

type stylesheet = Stylesheet of rule list
[@@deriving show { with_path = false }]
