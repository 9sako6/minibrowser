open Node

(*
  Calculate the width of a block-level non-replaced element in normal flow.
  Sets the horizontal margin, padding, border, and the `width`.
*)
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
  let total =
    [
      width;
      padding_left;
      padding_right;
      border_left;
      border_right;
      margin_left;
      margin_right;
    ]
    |> List.map Css.Value.get_size_value
    |> List.fold_left ( +. ) 0.
  in
  (*
    If 'width' is not 'auto' and 'border-left-width' + 'padding-left' + 'width' + 'padding-right' + 'border-right-width'
    (plus any of 'margin-left' or 'margin-right' that are not 'auto') is larger than the width of the containing block,
    then any 'auto' values for 'margin-left' or 'margin-right' are, for the following rules, treated as zero.
  *)
  let margin_left, margin_right =
    match (width = auto, total > block.box.rect.width) with
    | true, true ->
        let margin_left =
          if margin_left = auto then Css.Value.Size (0., Px) else margin_left
        in
        let margin_right =
          if margin_right = auto then Css.Value.Size (0., Px) else margin_right
        in
        (margin_left, margin_right)
    | _ -> (margin_left, margin_right)
  in
  let underflow = block.box.rect.width -. total in
  let width, margin_left, margin_right =
    match (width = auto, margin_left = auto, margin_right = auto) with
    (* If the values are overconstrained, calculate margin_right. *)
    | false, false, false ->
        (width, margin_left, Css.Value.(margin_right + Size (underflow, Px)))
    (* If exactly one size is auto, its used value follows from the equality. *)
    | false, true, false -> (width, Css.Value.Size (underflow, Px), margin_right)
    | false, false, true -> (width, margin_left, Css.Value.Size (underflow, Px))
    (* If margin-left and margin-right are both auto, their used values are equal. *)
    | false, true, true ->
        ( width,
          Css.Value.Size (underflow /. 2., Px),
          Css.Value.Size (underflow /. 2., Px) )
    | true, _, _ ->
        let margin_left =
          if margin_left = auto then Css.Value.Size (0., Px) else margin_left
        in
        let margin_right =
          if margin_right = auto then Css.Value.Size (0., Px) else margin_right
        in
        let width, margin_right =
          match underflow > 0. with
          | true -> (Css.Value.Size (underflow, Px), margin_right)
          (* Width can't be negative. Adjust the right margin instead. *)
          | false -> (width, Css.Value.Size (-.underflow, Px))
        in
        (width, margin_left, margin_right)
  in

  let rect = { block.box.rect with width = Css.Value.get_size_value width } in
  let padding =
    {
      block.box.padding with
      left = Css.Value.get_size_value padding_left;
      right = Css.Value.get_size_value padding_right;
    }
  in
  let border =
    {
      block.box.border with
      left = Css.Value.get_size_value border_left;
      right = Css.Value.get_size_value border_right;
    }
  in
  let margin =
    {
      block.box.margin with
      left = Css.Value.get_size_value margin_left;
      right = Css.Value.get_size_value margin_right;
    }
  in
  let box = { rect; padding; border; margin } in
  { block with box }

let height_calculated_block block =
  let style_map = !(block.style_ref).specified_values in
  let height =
    try Css.Value_map.find "height" style_map |> Css.Value.get_size_value
    with Not_found -> block.box.rect.height
  in
  let rect = { block.box.rect with height } in
  let box = { block.box with rect } in
  { block with box }

let position_calculated_block block = block

let empty ?(width = 0.) ?(height = 0.) style_ref =
  let rect = { x = 0.; y = 0.; width; height } in
  let padding = { top = 0.; right = 0.; bottom = 0.; left = 0. } in
  let border = { top = 0.; right = 0.; bottom = 0.; left = 0. } in
  let margin = { top = 0.; right = 0.; bottom = 0.; left = 0. } in
  let box = { rect; padding; border; margin } in
  let box_type = Anonymous in
  { box; box_type; style_ref; children = [] }

(* Build layout tree from style tree. *)
let rec build ?(parent_width = 0.) ?(parent_height = 0.) style =
  match style with
  | Style_node.{ node = _; specified_values; children } as style_node ->
      let block =
        empty ~width:parent_width ~height:parent_height (ref style_node)
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
