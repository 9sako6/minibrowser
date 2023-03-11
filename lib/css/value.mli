exception Invalid_size_value of string
exception Invalid_value of string

type size_unit = Px [@@deriving show { with_path = false }]

type t =
  | Keyword of string
  | Size of float * size_unit
  | Rgb of int * int * int
[@@deriving show { with_path = false }]

val get_size_value : t -> float
val ( + ) : t -> t -> t
val build : string list -> t
