module Attribute : sig
  type t [@@deriving show { with_path = false }]

  val build : string -> string -> t
  val name : t -> string
  val value : t -> string
end = struct
  type t = string * string [@@deriving show { with_path = false }]

  let build key value = (key, value)

  let name = function
    | name, _ -> name

  let value = function
    | _, value -> value
end

type t =
  | Element of string * Attribute.t list * t list
  | InnerText of string
[@@deriving show { with_path = false }]

let empty = InnerText ""
