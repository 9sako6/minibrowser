type selector =
  | Universal_selector
  | Class_selector of string

type declaration = Declaration of string * string
type rule = Rule of selector list * declaration list
type stylesheet = Stylesheet of rule list
