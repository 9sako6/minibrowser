type color = int * int * int

type rect = {
  x : int;
  y : int;
  width : int;
  height : int;
}

type t = color * rect

let string_of_rect rect =
  match rect with
  | { x; y; width; height } ->
      Printf.sprintf "{x=%d; y=%d; width=%d; height=%d;}" x y width height

let rect_of_box box =
  match (Layout_box.Box.border_box box).rect with
  | { x; y; width; height } ->
      {
        x = int_of_float x;
        y = int_of_float y;
        width = int_of_float width;
        height = int_of_float height;
      }

let%expect_test "rect_of_box" =
  let box =
    Layout_box.Box.
      {
        rect = { x = 12.00; y = 12.00; width = 0.00; height = 24.00 };
        padding = { top = 12.00; right = 12.00; bottom = 12.00; left = 12.00 };
        border = { top = 0.00; right = 0.00; bottom = 0.00; left = 0.00 };
        margin = { top = 0.00; right = -24.00; bottom = 0.00; left = 0.00 };
      }
  in
  box |> Layout_box.Box.border_box |> Layout_box.Box.string_of_box
  |> print_endline;
  box |> rect_of_box |> string_of_rect |> print_endline;
  [%expect {||}]

let rec build layout_box =
  let color = Layout_box.Block.get_background_color layout_box in
  match layout_box with
  | Layout_box.Node.{ box; box_type = _; style_ref = _; children } ->
      let rect = rect_of_box box in
      rect |> string_of_rect |> print_endline;
      let command = (color, rect) in
      let children = List.map build children in
      command :: List.flatten children
