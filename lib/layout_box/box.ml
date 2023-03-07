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

type t = {
  rect : rect;
  padding : edge;
  border : edge;
  margin : edge;
}

let string_of_rect rect =
  Printf.sprintf "{x = %.2f; y = %.2f; width = %.2f; height = %.2f;}" rect.x
    rect.y rect.width rect.height

let%expect_test "string_of_rect" =
  { x = 123.; y = 23.1; width = 120.; height = 24. }
  |> string_of_rect |> print_endline;
  [%expect {| {x = 123.00; y = 23.10; width = 120.00; height = 24.00;} |}]

let string_of_edge edge =
  Printf.sprintf "{top = %.2f; right = %.2f; bottom = %.2f; left = %.2f;}"
    edge.top edge.right edge.bottom edge.left

let%expect_test "string_of_edge" =
  { top = 123.; right = 23.1; bottom = 120.; left = 24. }
  |> string_of_edge |> print_endline;
  [%expect {| {top = 123.00; right = 23.10; bottom = 120.00; left = 24.00;} |}]

let string_of_box ?(indent = "") box =
  let rect_string = box.rect |> string_of_rect in
  let padding_string = box.padding |> string_of_edge in
  let border_string = box.border |> string_of_edge in
  let margin_string = box.margin |> string_of_edge in
  Printf.sprintf
    "%s{\n\
     %s  rect = %s\n\
     %s  padding = %s\n\
     %s  border = %s\n\
     %s  margin = %s\n\
     %s}"
    indent indent rect_string indent padding_string indent border_string indent
    margin_string indent

let%expect_test "string_of_box" =
  {
    rect = { x = 123.; y = 23.1; width = 120.; height = 24. };
    padding = { top = 123.; right = 23.1; bottom = 120.; left = 24. };
    border = { top = 123.; right = 23.1; bottom = 120.; left = 24. };
    margin = { top = 123.; right = 23.1; bottom = 120.; left = 24. };
  }
  |> string_of_box |> print_endline;
  [%expect
    {|
    {
      rect = {x = 123.00; y = 23.10; width = 120.00; height = 24.00;}
      padding = {top = 123.00; right = 23.10; bottom = 120.00; left = 24.00;}
      border = {top = 123.00; right = 23.10; bottom = 120.00; left = 24.00;}
      margin = {top = 123.00; right = 23.10; bottom = 120.00; left = 24.00;}
    }
  |}]

let empty ?(width = 0.) ?(height = 0.) () =
  let rect = { x = 0.; y = 0.; width; height } in
  let padding = { top = 0.; right = 0.; bottom = 0.; left = 0. } in
  let border = { top = 0.; right = 0.; bottom = 0.; left = 0. } in
  let margin = { top = 0.; right = 0.; bottom = 0.; left = 0. } in
  { rect; padding; border; margin }

(*
  Calculate the width of a block-level non-replaced element in normal flow.
  Sets the horizontal margin, padding, border, and the `width`.
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
    (empty ~width:100. ())
  |> string_of_box |> print_endline;

  [%expect
    {|
      {
        rect = {x = 0.00; y = 0.00; width = 100.00; height = 0.00;}
        padding = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
        border = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
        margin = {top = 0.00; right = 0.00; bottom = 0.00; left = 0.00;}
      }
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
  { rect; padding; border; margin }
  |> margin_box |> string_of_box |> print_endline;
  [%expect
    {| 
      {
        rect = {x = -107.00; y = -107.00; width = 224.00; height = 224.00;}
        padding = {top = 2.00; right = 2.00; bottom = 2.00; left = 2.00;}
        border = {top = 5.00; right = 5.00; bottom = 5.00; left = 5.00;}
        margin = {top = 100.00; right = 100.00; bottom = 100.00; left = 100.00;}
      }
  |}]
