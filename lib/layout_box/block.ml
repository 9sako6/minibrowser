open Node

let width_calculated_block block =
  let style_map = !(block.style_ref).specified_values in
  let auto = Css.Value.Keyword "auto" in
  let width =
    try Css.Value_map.find "width" style_map with Not_found -> auto
  in
  let padding_left =
    Css.Value_map.lookup
      [ "padding-left"; "padding" ]
      Css.Value.(Size (0., Px))
      style_map
  in
  let padding_right =
    Css.Value_map.lookup
      [ "padding-right"; "padding" ]
      Css.Value.(Size (0., Px))
      style_map
  in
  let border_left =
    Css.Value_map.lookup
      [ "border-left-width"; "border" ]
      Css.Value.(Size (0., Px))
      style_map
  in
  let border_right =
    Css.Value_map.lookup
      [ "border-right-width"; "border" ]
      Css.Value.(Size (0., Px))
      style_map
  in
  let margin_left =
    Css.Value_map.lookup
      [ "margin-left"; "margin" ]
      Css.Value.(Size (0., Px))
      style_map
  in
  let margin_right =
    Css.Value_map.lookup
      [ "margin-right"; "margin" ]
      Css.Value.(Size (0., Px))
      style_map
  in
  let box =
    Box.width_calculated_box ~width ~padding_left ~padding_right ~border_left
      ~border_right ~margin_left ~margin_right block.box
  in
  { block with box }

let height_calculated_block block =
  let style_map = !(block.style_ref).specified_values in
  let height =
    try Css.Value_map.find "height" style_map
    with Not_found -> Css.Value.Size (block.box.rect.height, Px)
  in
  let box = Box.height_calculated_box ~height block.box in
  { block with box }

let position_calculated_block block = block

(* Build layout tree from style tree. *)
let rec build ?(parent_width = 0.) ?(parent_height = 0.) style =
  match style with
  | Style_node.{ node = _; specified_values; children } as style_node ->
      let block =
        {
          box = Box.empty ~width:parent_width ~height:parent_height ();
          box_type = Anonymous;
          style_ref = ref style_node;
          children = [];
        }
      in
      let box_type =
        try
          match Css.Value_map.find "display" specified_values with
          | Css.Value.Keyword "block" -> Block
          | _ -> Inline
        with Not_found -> Inline
      in
      let block = width_calculated_block block in
      let block = height_calculated_block block in
      {
        block with
        box_type;
        children =
          List.map
            (build ~parent_width:block.box.rect.width
               ~parent_height:block.box.rect.height)
            children;
      }

let%expect_test "build" =
  let dom_nodes =
    "<div class=\"container\"><p>alice</p><p>bob</p></div>"
    |> Dom.Tokenizer.tokenize |> Dom.Parser.parse
  in
  let css =
    ".container {display: block; width: 200px; height: 100px;}"
    |> Css.Tokenizer.tokenize |> Css.Parser.parse
  in
  let style_nodes =
    dom_nodes |> List.map ref |> List.map (Style_node.build css)
  in
  let layout_nodes = style_nodes |> List.map build in
  layout_nodes |> List.map to_string |> List.iter print_endline;
  [%expect
    {|
      Element("div") = Block
      {
        rect = {x = 0.00; y = 0.00; width = 200.00; height = 100.00;}
        padding = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
        border = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
        margin = {top = 0.00; right = -200.00; bottom = 0.00; left = 0.00;}
      }
        Element("p") = Inline
        {
          rect = {x = 0.00; y = 0.00; width = 200.00; height = 100.00;}
          padding = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
          border = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
          margin = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
        }
          InnerText("alice") = Inline
          {
            rect = {x = 0.00; y = 0.00; width = 200.00; height = 100.00;}
            padding = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
            border = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
            margin = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
          }

        Element("p") = Inline
        {
          rect = {x = 0.00; y = 0.00; width = 200.00; height = 100.00;}
          padding = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
          border = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
          margin = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
        }
          InnerText("bob") = Inline
          {
            rect = {x = 0.00; y = 0.00; width = 200.00; height = 100.00;}
            padding = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
            border = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
            margin = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
          }
  |}]
