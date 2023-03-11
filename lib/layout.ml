type rect = {
  x : float;
  y : float;
  width : float;
  height : float;
}
[@@deriving show { with_path = false }]

type edge = {
  top : float;
  right : float;
  bottom : float;
  left : float;
}
[@@deriving show { with_path = false }]

type box = {
  rect : rect;
  padding : edge;
  border : edge;
  margin : edge;
}
[@@deriving show { with_path = false }]

let empty_box ?(width = 0.) ?(height = 0.) () =
  let rect = { x = 0.; y = 0.; width; height } in
  let padding = { top = 0.; right = 0.; bottom = 0.; left = 0. } in
  let border = { top = 0.; right = 0.; bottom = 0.; left = 0. } in
  let margin = { top = 0.; right = 0.; bottom = 0.; left = 0. } in
  { rect; padding; border; margin }

type box_type =
  | Inline
  | Block
  | Anonymous

type color = int * int * int

let default_color = (0, 0, 0)

type t = {
  box : box;
  box_type : box_type;
  style_ref : Style_tree.Node.t ref;
  children : t list;
  color : color;
}

(*
  > 10.3.3 Block-level, non-replaced elements in normal flow
  > The following constraints must hold among the used values of the other properties:
  > 
  > 'margin-left' + 'border-left-width' + 'padding-left' + 'width' + 'padding-right' + 'border-right-width' + 'margin-right' = width of containing block.

  https://www.w3.org/TR/CSS2/visudet.html#blockwidth
*)
let width_calculated_box ~width ~padding_left ~padding_right ~border_left
    ~border_right ~margin_left ~margin_right box =
  let open Css.Value in
  let auto = Keyword "auto" in
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
    |> List.map get_size_value |> List.fold_left ( +. ) 0.
  in
  (*
    If 'width' is not 'auto' and 'border-left-width' + 'padding-left' + 'width' + 'padding-right' + 'border-right-width'
    (plus any of 'margin-left' or 'margin-right' that are not 'auto') is larger than the width of the containing block,
    then any 'auto' values for 'margin-left' or 'margin-right' are, for the following rules, treated as zero.
  *)
  let margin_left, margin_right =
    match (width = auto, total > box.rect.width) with
    | true, true ->
        let margin_left =
          if margin_left = auto then Size (0., Px) else margin_left
        in
        let margin_right =
          if margin_right = auto then Size (0., Px) else margin_right
        in
        (margin_left, margin_right)
    | _ -> (margin_left, margin_right)
  in
  let underflow = box.rect.width -. total in
  let width, margin_left, margin_right =
    match (width = auto, margin_left = auto, margin_right = auto) with
    (* If the values are overconstrained, calculate margin_right. *)
    | false, false, false ->
        (width, margin_left, margin_right + Size (underflow, Px))
    (* If exactly one size is auto, its used value follows from the equality. *)
    | false, true, false -> (width, Size (underflow, Px), margin_right)
    | false, false, true -> (width, margin_left, Size (underflow, Px))
    (* If margin-left and margin-right are both auto, their used values are equal. *)
    | false, true, true ->
        (width, Size (underflow /. 2., Px), Size (underflow /. 2., Px))
    | true, _, _ ->
        let margin_left =
          if margin_left = auto then Size (0., Px) else margin_left
        in
        let margin_right =
          if margin_right = auto then Size (0., Px) else margin_right
        in
        let width, margin_right =
          match underflow >= 0. with
          | true -> (Size (underflow, Px), margin_right)
          (* Width can't be negative. Adjust the right margin instead. *)
          | false -> (Size (0., Px), margin_right + Size (underflow, Px))
        in
        (width, margin_left, margin_right)
  in

  let rect = { box.rect with width = get_size_value width } in
  let padding =
    {
      box.padding with
      left = get_size_value padding_left;
      right = get_size_value padding_right;
    }
  in
  let border =
    {
      box.border with
      left = get_size_value border_left;
      right = get_size_value border_right;
    }
  in
  let margin =
    {
      box.margin with
      left = get_size_value margin_left;
      right = get_size_value margin_right;
    }
  in
  { rect; padding; border; margin }

let%expect_test "width_calculated_box" =
  width_calculated_box
    ~width:(Size (100., Px))
    ~padding_left:(Size (0., Px))
    ~padding_right:(Size (0., Px))
    ~border_left:(Size (0., Px))
    ~border_right:(Size (0., Px))
    ~margin_left:(Size (0., Px))
    ~margin_right:(Size (0., Px))
    (empty_box ~width:100. ())
  |> show_box |> print_endline;

  [%expect
    {|
      { rect = { x = 0.; y = 0.; width = 100.; height = 0. };
        padding = { top = 0.; right = 0.; bottom = 0.; left = 0. };
        border = { top = 0.; right = 0.; bottom = 0.; left = 0. };
        margin = { top = 0.; right = 0.; bottom = 0.; left = 0. } }
  |}]

let expanded_by edge rect =
  {
    x = rect.x -. edge.left;
    y = rect.y -. edge.top;
    width = rect.width +. edge.left +. edge.right;
    height = rect.height +. edge.top +. edge.bottom;
  }

let padding_box box = { box with rect = expanded_by box.padding box.rect }

let border_box box =
  { box with rect = expanded_by box.border (padding_box box).rect }

let margin_box box =
  { box with rect = expanded_by box.margin (border_box box).rect }

let%expect_test "margin_box" =
  let rect = { x = 0.; y = 0.; width = 10.; height = 10. } in
  let padding = { top = 2.; right = 2.; bottom = 2.; left = 2. } in
  let border = { top = 5.; right = 5.; bottom = 5.; left = 5. } in
  let margin = { top = 100.; right = 100.; bottom = 100.; left = 100. } in
  { rect; padding; border; margin } |> margin_box |> show_box |> print_endline;
  [%expect
    {| 
      { rect = { x = -107.; y = -107.; width = 224.; height = 224. };
        padding = { top = 2.; right = 2.; bottom = 2.; left = 2. };
        border = { top = 5.; right = 5.; bottom = 5.; left = 5. };
        margin = { top = 100.; right = 100.; bottom = 100.; left = 100. } }
  |}]

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
    width_calculated_box ~width ~padding_left ~padding_right ~border_left
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

let position_calculated_block block containing_block =
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
  let x =
    containing_block.box.rect.x +. padding.left +. border.left +. margin.left
  in
  let y =
    containing_block.box.rect.height +. containing_block.box.rect.y
    +. padding.top +. border.top +. margin.top
  in
  print_endline (Printf.sprintf "(%f, %f)" x y);
  let rect = { block.box.rect with x; y } in
  let box = { rect; padding; border; margin } in
  { block with box }

let empty ?(width = 0.) ?(height = 0.) () =
  let box = empty_box ~width ~height () in
  {
    box;
    box_type = Anonymous;
    children = [];
    style_ref = ref Style_tree.Node.empty;
    color = default_color;
  }

(* Build layout tree from style tree. *)
let rec build ?(containing_block = empty ()) style =
  match style with
  | Style_tree.Node.{ node = _; specified_values; children } as style_node ->
      let color =
        try
          match Css.Value_map.find "background-color" specified_values with
          | Css.Value.Rgb (r, g, b) -> (r, g, b)
          | _ -> raise Not_found
        with Not_found -> default_color
      in
      let block =
        {
          box =
            empty_box ~width:containing_block.box.rect.width
              ~height:containing_block.box.rect.height ();
          box_type = Anonymous;
          style_ref = ref style_node;
          children = [];
          color;
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
      let block = position_calculated_block block containing_block in
      let children = List.map (build ~containing_block:block) children in
      let rec height_aux blocks acc =
        match blocks with
        | [] -> acc
        | head :: rest ->
            let height = (margin_box head.box).rect.height in
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

(* let%expect_test "build" =
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
       style_nodes |> List.map (build ~parent_box:(empty_box ~width:200. ()))
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

   let%expect_test "build" =
     let dom_nodes =
       "<div><div></div></div>" |> Dom.Tokenizer.tokenize |> Dom.Parser.parse
     in
     let css =
       "* {  display: block; padding: 12px;}" |> Css.Tokenizer.tokenize
       |> Css.Parser.parse
     in
     let style_nodes =
       dom_nodes |> List.map ref |> List.map (Style_tree.Node.build css)
     in
     let layout_nodes =
       style_nodes |> List.map (build ~parent_box:(Box.empty ()))
     in
     layout_nodes |> List.map to_string |> List.iter print_endline;
     [%expect
       {|
            Element("div") = Block
            {
              rect = {x = 12.00; y = 12.00; width = 0.00; height = 24.00;}
              padding = {top = 12.00; right = 12.00; bottom = 12.00; left = 12.00;}
              border = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
              margin = {top = 0.00; right = -24.00; bottom = 0.00; left = 0.00;}
            }
              Element("div") = Block
              {
                rect = {x = 12.00; y = 12.00; width = 0.00; height = 0.00;}
                padding = {top = 12.00; right = 12.00; bottom = 12.00; left = 12.00;}
                border = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
                margin = {top = 0.00; right = -24.00; bottom = 0.00; left = 0.00;}
              }
        |}] *)
