module Attribute : sig
  type t

  val create : string -> string -> t
  val to_string : t -> string
end = struct
  type t = string * string

  let create key value = (key, value)

  let to_string attribute =
    match attribute with
    | key, value -> Printf.sprintf "%s=\"%s\"" key value
end

type t =
  | Element of string * Attribute.t list * t list
  | InnerText of string
