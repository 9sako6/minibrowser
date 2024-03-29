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

type t = {
  box : box;
  style_ref : Style.t ref;
  children : t list;
}

let empty_box ?(width = 0.) ?(height = 0.) () =
  let rect = { x = 0.; y = 0.; width; height } in
  let padding = { top = 0.; right = 0.; bottom = 0.; left = 0. } in
  let border = { top = 0.; right = 0.; bottom = 0.; left = 0. } in
  let margin = { top = 0.; right = 0.; bottom = 0.; left = 0. } in
  { rect; padding; border; margin }

let empty ?(width = 0.) ?(height = 0.) () =
  let box = empty_box ~width ~height () in
  { box; children = []; style_ref = ref (Style.empty ()) }

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

(*
  > 10.3.3 Block-level, non-replaced elements in normal flow
  > The following constraints must hold among the used values of the other properties:
  > 
  > 'margin-left' + 'border-left-width' + 'padding-left' + 'width' + 'padding-right' + 'border-right-width' + 'margin-right' = width of containing block.

  https://www.w3.org/TR/CSS2/visudet.html#blockwidth
*)
let width_calculated_layout layout =
  let style = !(layout.style_ref) in
  let box = layout.box in
  let width = style.props.width in
  let padding_left = style.props.padding_left in
  let padding_right = style.props.padding_right in
  let border_left = style.props.border_left in
  let border_right = style.props.border_right in
  let margin_left = style.props.margin_left in
  let margin_right = style.props.margin_right in
  let open Style in
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
    match (width = Auto, total > box.rect.width) with
    | true, true ->
        let margin_left = if margin_left = Auto then Px 0. else margin_left in
        let margin_right =
          if margin_right = Auto then Px 0. else margin_right
        in
        (margin_left, margin_right)
    | _ -> (margin_left, margin_right)
  in
  let underflow = box.rect.width -. total in
  let width, margin_left, margin_right =
    match (width = Auto, margin_left = Auto, margin_right = Auto) with
    (* If the values are overconstrained, calculate margin_right. *)
    | false, false, false -> (width, margin_left, margin_right + Px underflow)
    (* If exactly one size is auto, its used value follows from the equality. *)
    | false, true, false -> (width, Px underflow, margin_right)
    | false, false, true -> (width, margin_left, Px underflow)
    (* If margin-left and margin-right are both auto, their used values are equal. *)
    | false, true, true -> (width, Px (underflow /. 2.), Px (underflow /. 2.))
    | true, _, _ ->
        let margin_left = if margin_left = Auto then Px 0. else margin_left in
        let margin_right =
          if margin_right = Auto then Px 0. else margin_right
        in
        let width, margin_right =
          match underflow >= 0. with
          | true -> (Px underflow, margin_right)
          (* Width can't be negative. Adjust the right margin instead. *)
          | false -> (Px 0., margin_right + Px underflow)
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
  let box = { rect; padding; border; margin } in
  { layout with box }

let height_calculated_layout layout =
  let style = !(layout.style_ref) in
  let height =
    match style.props.height with
    | Px px -> px
    | Auto -> layout.box.rect.height
  in
  let box = { layout.box with rect = { layout.box.rect with height } } in
  { layout with box }

let position_calculated_layout layout containing_layout =
  let style = !(layout.style_ref) in
  let calculate_padding style =
    let open Style in
    let padding_top =
      match style.props.padding_top with
      | Px px -> px
      | _ -> 0.
    in
    let padding_bottom =
      match style.props.padding_bottom with
      | Px px -> px
      | _ -> 0.
    in
    { layout.box.padding with top = padding_top; bottom = padding_bottom }
  in
  let calculate_border style =
    let open Style in
    let border_top =
      match style.props.border_top with
      | Px px -> px
      | _ -> 0.
    in
    let border_bottom =
      match style.props.border_bottom with
      | Px px -> px
      | _ -> 0.
    in
    { layout.box.border with top = border_top; bottom = border_bottom }
  in
  let calculate_margin style =
    let open Style in
    let margin_top =
      match style.props.margin_top with
      | Px px -> px
      | _ -> 0.
    in
    let margin_bottom =
      match style.props.margin_bottom with
      | Px px -> px
      | _ -> 0.
    in
    { layout.box.margin with top = margin_top; bottom = margin_bottom }
  in
  let padding = calculate_padding style in
  let border = calculate_border style in
  let margin = calculate_margin style in
  (* position *)
  let x =
    containing_layout.box.rect.x +. padding.left +. border.left +. margin.left
  in
  let y =
    containing_layout.box.rect.height +. containing_layout.box.rect.y
    +. padding.top +. border.top +. margin.top
  in
  let rect = { layout.box.rect with x; y } in
  let box = { rect; padding; border; margin } in
  { layout with box }

(* Build layout tree from style tree. *)
let rec build ?(containing_layout = empty ()) style =
  match style with
  | Style.{ children; props = _ } as style_node ->
      let layout =
        {
          box =
            empty_box ~width:containing_layout.box.rect.width
              ~height:containing_layout.box.rect.height ();
          style_ref = ref style_node;
          children = [];
        }
      in
      let layout = width_calculated_layout layout in
      let layout = position_calculated_layout layout containing_layout in
      let children = List.map (build ~containing_layout:layout) children in
      let children_height =
        List.fold_left
          (fun acc layout -> (margin_box layout.box).rect.height +. acc)
          0. children
      in
      let layout =
        {
          layout with
          box =
            {
              layout.box with
              rect = { layout.box.rect with height = children_height };
            };
        }
      in
      let layout = height_calculated_layout layout in
      { layout with children }

let build_layouts ~root_layout ~html ~css =
  let styles = Style.build_styles ~html ~css in
  styles |> List.map (build ~containing_layout:root_layout)
