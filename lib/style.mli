type t = {
  node : Dom.Node.t ref;
  specified_values : Css.Value_map.t;
  children : t list;
}

val empty : unit -> t
val build : Css.Node.stylesheet -> Dom.Node.t ref -> t
val get_background_color : t -> int * int * int
