type selector =
  | Universal_selector
  | Class_selector of string
[@@deriving show { with_path = false }]

type declaration = Declaration of string * Value.t
[@@deriving show { with_path = false }]

type rule = Rule of selector list * declaration list

type stylesheet = Stylesheet of rule list
[@@deriving show { with_path = false }]
