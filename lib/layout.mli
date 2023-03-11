type rect = {
  x : float;
  y : float;
  width : float;
  height : float;
}

type edge = {
  top : float;
  right : float;
  bottom : float;
  left : float;
}

type box = {
  rect : rect;
  padding : edge;
  border : edge;
  margin : edge;
}

type box_type =
  | Inline
  | Block
  | Anonymous

type color = int * int * int

type t = {
  box : box;
  box_type : box_type;
  style_ref : Style_tree.Node.t ref;
  children : t list;
  color : color;
}

val border_box : box -> box
val build : ?containing_block:t -> Style_tree.Node.t -> t
val empty : ?width:float -> ?height:float -> unit -> t
