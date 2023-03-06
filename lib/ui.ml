open Bogue

let main () =
  let b = Widget.check_box () in
  let l = Widget.label "Hello world" in
  let style = Style.Solid (Draw.black |> Draw.opaque) |> Style.of_bg in
  let box = Widget.box ~w:200 ~h:100 ~style () in
  let layout = Layout.flat_of_w [ b; l; box ] in

  let board = Bogue.of_layout layout in
  Bogue.run board

let build_from_layout_box layout_box =
  match layout_box with
  | Layout_box.Node.{ box; box_type = _; style_ref = _; children = _ } ->
      let width =
        box.rect.width +. box.padding.left +. box.padding.right
        +. box.border.left +. box.border.right +. box.margin.left
        +. box.margin.right
      in
      let height =
        box.rect.height +. box.padding.top +. box.padding.bottom
        +. box.border.top +. box.border.bottom +. box.margin.top
        +. box.margin.bottom
      in
      let style = Style.Solid (Draw.black |> Draw.opaque) |> Style.of_bg in
      let container =
        Widget.box ~w:(int_of_float width) ~h:(int_of_float height) ~style ()
      in
      Layout.flat_of_w [ container ]

let build html_string css_string =
  let dom_nodes = html_string |> Dom.Tokenizer.tokenize |> Dom.Parser.parse in
  let css = css_string |> Css.Tokenizer.tokenize |> Css.Parser.parse in
  let style_nodes =
    dom_nodes |> List.map ref |> List.map (Style_node.build css)
  in
  let layout_nodes = style_nodes |> List.map Layout_box.Block.build in
  let layouts = List.map build_from_layout_box layout_nodes in
  Bogue.of_layout (Layout.flat layouts)

let render board =
  Bogue.run board;
  Bogue.quit ()
