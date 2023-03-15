type rgb = float * float * float

type rect = {
  x : float;
  y : float;
  width : float;
  height : float;
}
[@@deriving show { with_path = false }]

type t = Rect of rgb * rect

let rect_of_box box =
  match (Layout.border_box box).rect with
  | { x; y; width; height } -> { x; y; width; height }

let%expect_test "rect_of_box" =
  let box =
    Layout.
      {
        rect = { x = 12.00; y = 12.00; width = 0.00; height = 24.00 };
        padding = { top = 12.00; right = 12.00; bottom = 12.00; left = 12.00 };
        border = { top = 0.00; right = 0.00; bottom = 0.00; left = 0.00 };
        margin = { top = 0.00; right = -24.00; bottom = 0.00; left = 0.00 };
      }
  in
  box |> rect_of_box |> show_rect |> print_endline;
  [%expect {| { x = 0.; y = 0.; width = 24.; height = 48. } |}]

let build ~max_width ~html ~css =
  let root_layout = Layout.empty ~width:max_width () in
  let layouts = Layout.build_layouts ~root_layout ~html ~css in
  let rec aux layouts acc =
    match layouts with
    | [] -> acc
    | Layout.{ box; style_ref = _; children; color = r, g, b } :: rest ->
        let rgb = (float r /. 255., float g /. 255., float b /. 255.) in
        let rect = rect_of_box box in
        let command = Rect (rgb, rect) in
        let children_commands = aux children [] in
        aux rest (command :: children_commands)
  in
  aux layouts []
