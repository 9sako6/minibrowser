type t = {
  node : Dom.Node.t ref;
  specified_values : Css.Value_map.t;
  children : t list;
}

val empty : unit -> t
val build_styles : html:string -> css:string -> t list
val get_background_color : t -> int * int * int
