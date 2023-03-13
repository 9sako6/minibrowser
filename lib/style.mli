type t = {
  node : Dom.Node.t ref;
  specified_values : Css.Value_map.t;
  children : t list;
}

val empty : unit -> t
val build_styles : html:string -> css:string -> t list
val get_background_color : t -> int * int * int
val get_size : deafult:float -> key:string -> t -> float
val lookup : default:float -> keys:string list -> t -> float
val to_string : ?indent:string -> t -> string
