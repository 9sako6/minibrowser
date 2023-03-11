open Css.Value

let%expect_test "( + )" =
  Size (10., Px) + Size (2., Px) |> show |> print_endline;
  [%expect {| (Size (12., Px)) |}]

let%expect_test "( + )" =
  Size (10., Px) + Keyword "auto" |> show |> print_endline;
  [%expect {| (Size (10., Px)) |}]

let%expect_test "build" =
  [ "12px" ] |> build |> show |> print_endline;
  [%expect {| (Size (12., Px)) |}]

let%expect_test "build value of a hex color code" =
  [ "#"; "bb22FF" ] |> build |> show |> print_endline;
  [%expect {| (Rgb (187, 34, 255)) |}]

let%expect_test "build" =
  [ "inline" ] |> build |> show |> print_endline;
  [%expect {| (Keyword "inline") |}]
