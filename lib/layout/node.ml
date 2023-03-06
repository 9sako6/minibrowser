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

type box = {
  rect : rect;
  padding : edge;
  border : edge;
  margin : edge;
}

type box_type =
  | Inline
  | Block
  | Anonymous

type t = {
  box : box;
  box_type : box_type;
  style_ref : Style.t ref;
  children : t list;
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

let string_of_box box =
  let rect_string = box.rect |> string_of_rect in
  let padding_string = box.padding |> string_of_edge in
  let border_string = box.border |> string_of_edge in
  let margin_string = box.margin |> string_of_edge in
  Printf.sprintf
    "{\n  rect = %s\n  padding = %s\n  border = %s\n  margin = %s\n}"
    rect_string padding_string border_string margin_string

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
