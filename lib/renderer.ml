open Bogue

let background_color ~r = Layout.opaque_bg (r, 100, 100)

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
            | Block ->
                Layout.tower ~scale_content:false
                  ~background:(background_color ~r:(Random.int 255))
                  children_layout_boxes
            | Inline ->
                Layout.flat ~scale_content:false ~margins:0
                  children_layout_boxes
            | Anonymous ->
                Layout.tower ~scale_content:false children_layout_boxes)
        | InnerText text -> (
            let text_label =
              Widget.label text |> Layout.resident ~w:width ~h:height
            in
            match box_type with
            | Block ->
                Layout.tower ~scale_content:false
                  ([ text_label ] @ children_layout_boxes)
            | Inline ->
                Layout.flat ~scale_content:false ~margins:0
                  ([ text_label ] @ children_layout_boxes)
            | Anonymous ->
                Layout.tower ~scale_content:false
                  ([ text_label ] @ children_layout_boxes))
      in
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

let render html_string css_string =
  let board = build html_string css_string in
  Bogue.run board;
  Bogue.quit ()
