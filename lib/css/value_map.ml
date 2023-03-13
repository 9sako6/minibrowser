module M = Map.Make (String)
include M

type t = Value.t M.t

let pp map = [%show: (string * Value.t) list] (M.bindings map)

let rec lookup keys default_value map =
  match keys with
  | [] -> default_value
  | key :: rest -> (
      try find key map with Not_found -> lookup rest default_value map)
