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

type color = int * int * int

type t = {
  box : box;
  style_ref : Style.t ref;
  children : t list;
  color : color;
}

val empty : ?width:float -> ?height:float -> unit -> t
val border_box : box -> box
val build_layouts : root_layout:t -> html:string -> css:string -> t list
