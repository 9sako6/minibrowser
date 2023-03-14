type t = {
  node : Dom.Node.t ref;
  specified_values : Css.Value_map.t;
  children : t list;
  size : size;
}

and size = {
  width : px;
  height : px;
  padding : px;
  padding_top : px;
  padding_right : px;
  padding_bottom : px;
  padding_left : px;
  border : px;
  border_top : px;
  border_right : px;
  border_bottom : px;
  border_left : px;
  margin : px;
  margin_top : px;
  margin_right : px;
  margin_bottom : px;
  margin_left : px;
}

and px =
  | Px of float
  | Auto

val empty : unit -> t
val build_styles : html:string -> css:string -> t list
val get_background_color : t -> int * int * int
val to_string : ?indent:string -> t -> string
val get_size_value : px -> float
val ( + ) : px -> px -> px
