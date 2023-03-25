module M = Map.Make (String)
include M

type t = Value.t M.t

let pp fmt value_map =
  let open Format in
  let pp_binding fmt (key, value) = fprintf fmt "%s -> %a" key Value.pp value in
  let bindings = bindings value_map in
  pp_print_list
    ~pp_sep:(fun fmt () -> fprintf fmt ";@ ")
    pp_binding fmt bindings

let show value_map = Format.asprintf "%a" pp value_map

let rec lookup keys default_value map =
  match keys with
  | [] -> default_value
  | key :: rest -> (
      try find key map with Not_found -> lookup rest default_value map)
