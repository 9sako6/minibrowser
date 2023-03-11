type t

val empty : t
val find : string -> t -> Value.t
val bindings : t -> (string * Value.t) list
val add : string -> Value.t -> t -> t
val lookup : string list -> Value.t -> t -> Value.t
val pp : t -> string
