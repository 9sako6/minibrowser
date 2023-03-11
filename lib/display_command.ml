type color = int * int * int [@@deriving show { with_path = false }]

type rect = {
  x : int;
  y : int;
  width : int;
  height : int;
}
[@@deriving show { with_path = false }]

type t = color * rect [@@deriving show { with_path = false }]

let rect_of_box box =
  match (Layout.border_box box).rect with
  | { x; y; width; height } ->
      {
        x = int_of_float x;
        y = int_of_float y;
        width = int_of_float width;
        height = int_of_float height;
      }

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
  [%expect {| { x = 0; y = 0; width = 24; height = 48 } |}]

let build_layouts ~html ~css =
  let dom_nodes = html |> Dom.Tokenizer.tokenize |> Dom.Parser.parse in
  let stylesheet = css |> Css.Tokenizer.tokenize |> Css.Parser.parse in
  let styles = dom_nodes |> List.map ref |> List.map (Style.build stylesheet) in
  let layout_root = Layout.empty ~width:200. () in
  styles |> List.map (Layout.build ~containing_block:layout_root)

let build ~html ~css =
  let layouts = build_layouts ~html ~css in
  let rec aux layouts acc =
    match layouts with
    | [] -> acc
    | Layout.{ box; box_type = _; style_ref = _; children; color } :: rest ->
        let rect = rect_of_box box in
        let command = (color, rect) in
        let children_commands = aux children [] in
        aux rest (command :: children_commands)
  in
  aux layouts []
