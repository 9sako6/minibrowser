type box_type =
  | Inline
  | Block
  | Anonymous

type t = {
  box : Box.t;
  box_type : box_type;
  style_ref : Style_tree.Node.t ref;
  children : t list;
}

let string_of_box_type = function
  | Inline -> "Inline"
  | Block -> "Block"
  | Anonymous -> "Anonymus"

let rec to_string ?(indent = "") = function
  | { box; box_type; style_ref; children } ->
      let node_string =
        match !(!style_ref.node) with
        | Dom.Node.Element (name, _, _) ->
            Printf.sprintf "%sElement(\"%s\")" indent name
        | Dom.Node.InnerText text ->
            Printf.sprintf "%sInnerText(\"%s\")" indent text
      in
      let box_string = Box.string_of_box ~indent box in
      let box_type_string = string_of_box_type box_type in
      let children_string =
        children
        |> List.map (to_string ~indent:(indent ^ "  "))
        |> String.concat "\n"
      in
      Printf.sprintf "%s = %s\n%s\n%s" node_string box_type_string box_string
        children_string
