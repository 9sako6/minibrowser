type name = string

type node =
  | Tag of name * node list
  | Text of string
