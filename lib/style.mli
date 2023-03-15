(* A styled DOM node with calculated size properties *)
type t = {
  node : Dom.Node.t ref; (* Reference to the corresponding DOM node *)
  specified_values : Css.Value_map.t; (* CSS property-value pairs *)
  props : props;
  children : t list; (* List of styled child nodes *)
}

and props = {
  display : display_type;
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

and display_type =
  | Inline
  | Block
  | Anonymous

val empty : unit -> t
val get_background_color : t -> int * int * int
val get_size_value : px -> float

(* Adds two size values, treating 'Auto' as 0.0 *)
val ( + ) : px -> px -> px

(* Builds a list of styled nodes from HTML and CSS strings *)
val build_styles : html:string -> css:string -> t list
