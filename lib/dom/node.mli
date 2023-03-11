module Attribute : sig
  type t [@@deriving show { with_path = false }]

  val build : string -> string -> t
  val name : t -> string
  val value : t -> string
end

type t =
  | Element of string * Attribute.t list * t list
  | InnerText of string
[@@deriving show { with_path = false }]

val empty : t
