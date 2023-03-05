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

let get_style_map block =
  match block.box_type with
  | Inline style_ref | Block style_ref -> !style_ref.specified_values
  | Anonymous -> Css.Value_map.empty

(*
  Calculate the width of a block-level non-replaced element in normal flow.
  Sets the horizontal margin, padding, border, and the `width`.
*)
(* let calculate_block_width block =
   let style_map = get_style_map block in
   let width =
     try Style.Value_map.find "width" style_map |> float_of_string
     with Not_found -> (* TODO *) 100.
   in
   let padding_left =
     Style.Value_map.lookup [ "padding-left"; "padding" ] "0" style_map
     |> float_of_string
   in
   let padding_right =
     Style.Value_map.lookup [ "padding-right"; "padding" ] "0" style_map
     |> float_of_string
   in
   let border_left =
     Style.Value_map.lookup [ "border-left-width"; "border" ] "0" style_map
     |> float_of_string
   in
   let border_right =
     Style.Value_map.lookup [ "border-right-width"; "border" ] "0" style_map
     |> float_of_string
   in
   let margin_left =
     Style.Value_map.lookup [ "margin-left"; "margin" ] "0" style_map
     |> float_of_string
   in
   let margin_right =
     Style.Value_map.lookup [ "margin-right"; "margin" ] "0" style_map
     |> float_of_string
   in
   List.fold_left ( +. ) 0.
     [
       width;
       padding_left;
       padding_right;
       border_left;
       border_right;
       margin_left;
       margin_right;
     ] *)

let calculate_block_height _block = 0.
let calculate_block_position _block = 0.

let rec create style =
  match style with
  | Style.{ node = _; specified_values = _; children } as style_node ->
      let rect = { x = 0.; y = 0.; width = 200.; height = 100. } in
      let padding = { top = 0.; right = 0.; bottom = 0.; left = 0. } in
      let border = { top = 0.; right = 0.; bottom = 0.; left = 0. } in
      let margin = { top = 0.; right = 0.; bottom = 0.; left = 0. } in
      let box = { rect; padding; border; margin } in
      (* let display_type = Style.Value_map.find "display" specified_values in *)
      let box_type =
        Inline (ref style_node)
        (* if display_type = "inline" then Inline (ref style_node)
           else Block (ref style_node) *)
      in
      { box; box_type; children = List.map create children }
