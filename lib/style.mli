(* A styled DOM node with calculated size properties *)
type t = {
  node : Dom.Node.t ref; (* Reference to the corresponding DOM node *)
  specified_values : Css.Value_map.t; (* CSS property-value pairs *)
  size : size; (* Calculated size properties *)
  children : t list; (* List of styled child nodes *)
}

(* Size properties for a styled DOM node *)
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

(* A size value in pixels or 'auto' *)
and px =
  | Px of float
  | Auto

(* Returns an empty styled node *)
val empty : unit -> t

(* Converts a styled node to a string with optional indentation *)
(* val to_string : ?indent:string -> t -> string *)

(* Builds a list of styled nodes from HTML and CSS strings *)
val build_styles : html:string -> css:string -> t list

(* Returns the background color of a styled node as an (r, g, b) tuple *)
val get_background_color : t -> int * int * int

(* Returns the float value of a size property, treating 'Auto' as 0.0 *)
val get_size_value : px -> float

(* Adds two size values, treating 'Auto' as 0.0 *)
val ( + ) : px -> px -> px
