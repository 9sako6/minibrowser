open Node

let width_calculated_block block =
  let style_map = !(block.style_ref).specified_values in
  let auto = Css.Value.Keyword "auto" in
  let zero = Css.Value.(Size (0., Px)) in
  let lookup keys = Css.Value_map.lookup keys zero style_map in
  let width =
    try Css.Value_map.find "width" style_map with Not_found -> auto
  in
  let padding_left = lookup [ "padding-left"; "padding" ] in
  let padding_right = lookup [ "padding-right"; "padding" ] in
  let border_left = lookup [ "border-left-width"; "border-width" ] in
  let border_right = lookup [ "border-right-width"; "border-width" ] in
  let margin_left = lookup [ "margin-left"; "margin" ] in
  let margin_right = lookup [ "margin-right"; "margin" ] in
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
  let box =
    {
      block.box with
      rect = { block.box.rect with height = Css.Value.get_size_value height };
    }
  in
  { block with box }

let position_calculated_block block =
  (* let box = Box.position_calculated_box block.box in *)
  let style_map = !(block.style_ref).specified_values in
  let zero = Css.Value.(Size (0., Px)) in
  let lookup keys = Css.Value_map.lookup keys zero style_map in
  let padding_top = lookup [ "padding-top"; "padding" ] in
  let padding_bottom = lookup [ "padding-bottom"; "padding" ] in
  let padding =
    {
      block.box.padding with
      top = Css.Value.get_size_value padding_top;
      bottom = Css.Value.get_size_value padding_bottom;
    }
  in
  let border_top = lookup [ "border-top-width"; "border-width" ] in
  let border_bottom = lookup [ "border-bottom-width"; "border-width" ] in
  let border =
    {
      block.box.border with
      top = Css.Value.get_size_value border_top;
      bottom = Css.Value.get_size_value border_bottom;
    }
  in
  let margin_top = lookup [ "margin-top"; "margin" ] in
  let margin_bottom = lookup [ "margin-bottom"; "margin" ] in
  let margin =
    {
      block.box.margin with
      top = Css.Value.get_size_value margin_top;
      bottom = Css.Value.get_size_value margin_bottom;
    }
  in
  let x = block.box.rect.x +. padding.left +. border.left +. margin.left in
  let y =
    block.box.rect.height +. block.box.rect.y +. padding.top +. border.top
    +. margin.top
  in
  let rect = { block.box.rect with x; y } in
  let box = Box.{ rect; padding; border; margin } in
  { block with box }

(* Build layout tree from style tree. *)
let rec build ?(parent_box = Box.empty ~width:0. ~height:0. ()) style =
  match style with
  | Style_tree.Node.{ node = _; specified_values; children } as style_node ->
      let block =
        {
          box =
            Box.empty ~width:parent_box.rect.width
              ~height:parent_box.rect.height ();
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
      let block = position_calculated_block block in
      let children = List.map (build ~parent_box:block.box) children in
      let rec height_aux blocks acc =
        match blocks with
        | [] -> acc
        | head :: rest ->
            let height = (Box.margin_box head.box).rect.height in
            height_aux rest (acc +. height)
      in
      let children_height = height_aux children 0. in
      let box =
        {
          block.box with
          rect = { block.box.rect with height = children_height };
        }
      in
      let block = { block with box } in
      let block = height_calculated_block block in
      { block with box_type; children }

let%expect_test "build" =
  let dom_nodes =
    "<div class=\"container\"><p class=\"name\">alice</p><p \
     class=\"name\">bob</p></div>" |> Dom.Tokenizer.tokenize |> Dom.Parser.parse
  in
  let css =
    ".container {display: block; width: 200px;} .name {height: 30px;}"
    |> Css.Tokenizer.tokenize |> Css.Parser.parse
  in
  let style_nodes =
    dom_nodes |> List.map ref |> List.map (Style_tree.Node.build css)
  in
  let layout_nodes =
    style_nodes |> List.map (build ~parent_box:(Box.empty ~width:200. ()))
  in
  layout_nodes |> List.map to_string |> List.iter print_endline;
  [%expect
    {|
      Element("div") = Block
      {
        rect = {x = 0.00; y = 0.00; width = 200.00; height = 60.00;}
        padding = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
        border = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
        margin = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
      }
        Element("p") = Inline
        {
          rect = {x = 0.00; y = 0.00; width = 200.00; height = 30.00;}
          padding = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
          border = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
          margin = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
        }
          InnerText("alice") = Inline
          {
            rect = {x = 0.00; y = 0.00; width = 200.00; height = 0.00;}
            padding = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
            border = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
            margin = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
          }

        Element("p") = Inline
        {
          rect = {x = 0.00; y = 0.00; width = 200.00; height = 30.00;}
          padding = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
          border = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
          margin = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
        }
          InnerText("bob") = Inline
          {
            rect = {x = 0.00; y = 0.00; width = 200.00; height = 0.00;}
            padding = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
            border = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
            margin = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
          }
  |}]
