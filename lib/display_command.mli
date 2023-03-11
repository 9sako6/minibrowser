type color = int * int * int

type rect = {
  x : int;
  y : int;
  width : int;
  height : int;
}

type t = color * rect [@@deriving show { with_path = false }]

val build : html:string -> css:string -> t list
