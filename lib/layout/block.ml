type rect = {
  x : float;
  y : float;
  width : float;
  height : float;
}

type edge = {
  top : float;
  right : float;
  bottom : float;
  left : float;
}

type box = {
  rect : rect;
  padding : edge;
  border : edge;
  margin : edge;
}

type box_type =
  | Inline of Style.t ref
  | Block of Style.t ref
  | Anonymous

type t = {
  box : box;
  box_type : box_type;
  children : t list;
}

let calculate_block_width _block = 0.
let calculate_block_height _block = 0.
let calculate_block_position _block = 0.

let rec create style =
  match style with
  | Style.{ node = _; specified_values; children } as style_node ->
      let rect = { x = 0.; y = 0.; width = 200.; height = 100. } in
      let padding = { top = 0.; right = 0.; bottom = 0.; left = 0. } in
      let border = { top = 0.; right = 0.; bottom = 0.; left = 0. } in
      let margin = { top = 0.; right = 0.; bottom = 0.; left = 0. } in
      let box = { rect; padding; border; margin } in
      let display_type = Style.Value_map.find "display" specified_values in
      let box_type =
        if display_type = "inline" then Inline (ref style_node)
        else Block (ref style_node)
      in
      { box; box_type; children = List.map create children }
