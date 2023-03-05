module Attribute : sig
  type t

  val build : string -> string -> t
  val name : t -> string
  val value : t -> string
  val to_string : t -> string
end = struct
  type t = string * string

  let build key value = (key, value)

  let name = function
    | name, _ -> name

  let value = function
    | _, value -> value

  let to_string attribute =
    match attribute with
    | key, value -> Printf.sprintf "%s=\"%s\"" key value
end

type t =
  | Element of string * Attribute.t list * t list
  | InnerText of string

let to_string node =
  let attributes_to_string attributes =
    attributes |> List.map Attribute.to_string |> String.concat " "
  in
  let rec nodes_to_string prefix nodes =
    match nodes with
    | [] -> ""
    | head :: rest ->
        Printf.sprintf "%s%s"
          (node_to_string prefix head)
          (nodes_to_string prefix rest)
  and node_to_string prefix = function
    | Element (name, attributes, children) ->
        let attrs = attributes_to_string attributes in
        Printf.sprintf "%s↳%s %s\n%s" prefix name attrs
          (nodes_to_string (prefix ^ "  ") children)
    | InnerText text -> Printf.sprintf "%s↳#text: %s\n" prefix text
  in
  node_to_string "" node

let%expect_test "to_string div tag with child" =
  Element
    ("div", [], [ InnerText "alice"; Element ("p", [], [ InnerText "child" ]) ])
  |> to_string |> print_endline;
  [%expect {|
  ↳div
    ↳#text: alice
    ↳p
      ↳#text: child
  |}]

let string_of_node = function
  | Element (name, attributes, _) ->
      let attributes_str =
        attributes |> List.map Attribute.to_string |> String.concat "; "
      in
      Printf.sprintf "Element(%s; [%s])" name attributes_str
  | InnerText text -> Printf.sprintf "InnerText(\"%s\")" text
