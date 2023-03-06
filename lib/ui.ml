open Bogue

let rec build_from_layout_box layout_box =
  match layout_box with
  | Layout_box.Node.{ box; box_type; style_ref; children } ->
      let width = box.rect.width |> int_of_float in
      let height = box.rect.height |> int_of_float in
      let children_layout_boxes = List.map build_from_layout_box children in
      let layout =
        match !(!style_ref.node) with
        | Element (_, _, _) -> (
            match box_type with
            | Block -> Layout.tower children_layout_boxes
            | Inline -> Layout.flat children_layout_boxes
            | Anonymous -> Layout.tower children_layout_boxes)
        | InnerText text -> (
            let text_label = Widget.label text |> Layout.resident in
            match box_type with
            | Block -> Layout.tower ([ text_label ] @ children_layout_boxes)
            | Inline -> Layout.flat ([ text_label ] @ children_layout_boxes)
            | Anonymous -> Layout.tower ([ text_label ] @ children_layout_boxes)
            )
      in
      Layout.set_size layout (width, height);
      layout

let build html_string css_string =
  let dom_nodes = html_string |> Dom.Tokenizer.tokenize |> Dom.Parser.parse in
  let css = css_string |> Css.Tokenizer.tokenize |> Css.Parser.parse in
  let style_nodes =
    dom_nodes |> List.map ref |> List.map (Style_tree.Node.build css)
  in
  let layout_nodes = style_nodes |> List.map Layout_box.Block.build in
  let layouts = List.map build_from_layout_box layout_nodes in
  Bogue.of_layout (Layout.flat layouts)

let render board =
  Bogue.run board;
  Bogue.quit ()
