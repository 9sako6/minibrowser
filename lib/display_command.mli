type rgb = float * float * float

type rect = {
  x : float;
  y : float;
  width : float;
  height : float;
}
[@@deriving show { with_path = false }]

type t = Rect of rgb * rect

val build : html:string -> css:string -> t list
