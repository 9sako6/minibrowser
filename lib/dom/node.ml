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

let empty = InnerText ""

  let rec string_of_node ?(with_children = true) ?(indent = "") = function
  | Element (name, attributes, children) ->
      let attributes_string =
        attributes |> List.map Attribute.to_string |> String.concat "; "
      in
      let children_string =
        if with_children then
          "\n"
          ^ (List.map
               (string_of_node ~with_children ~indent:(indent ^ "  "))
               children
            |> String.concat "\n")
        else ""
      in
      Printf.sprintf "%sElement(%s; [%s])%s" indent name attributes_string
        children_string
  | InnerText text -> Printf.sprintf "%sInnerText(\"%s\")" indent text

let%expect_test "string_of_node" =
  Element
    ("div", [ Attribute.build "class" "alert" ], [ Element ("div", [], []) ])
  |> string_of_node |> print_endline;
  [%expect {|
    Element(div; [class="alert"])
      Element(div; [])
  |}]

let%expect_test "string_of_node without children" =
  Element
    ("div", [ Attribute.build "class" "alert" ], [ Element ("div", [], []) ])
  |> string_of_node ~with_children:false
  |> print_endline;
  [%expect {|
    Element(div; [class="alert"])
  |}]

let%expect_test "string_of_node a div tag that has a child" =
  Element
    ("div", [], [ InnerText "alice"; Element ("p", [], [ InnerText "child" ]) ])
  |> string_of_node |> print_endline;
  [%expect
    {|
      Element(div; [])
        InnerText("alice")
        Element(p; [])
          InnerText("child")
  |}]
