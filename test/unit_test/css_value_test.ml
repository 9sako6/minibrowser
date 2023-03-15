open Css

let%test_module "Value_map.lookup" =
  (module struct
    let%expect_test "lookup" =
      let value = Value.Size (12., Px) in
      Value_map.empty
      |> Value_map.add "padding" value
      |> Value_map.lookup
           [ "padding-left"; "padding" ]
           (Value.Keyword "default")
      |> Value.show |> print_endline;
      [%expect {| (Size (12., Px)) |}]

    let%expect_test "lookup sets default value if a value doesn't exist" =
      Value_map.empty
      |> Value_map.lookup
           [ "padding-left"; "padding" ]
           (Value.Keyword "default")
      |> Value.show |> print_endline;
      [%expect {| (Keyword "default") |}]
  end)

let%test_module "Value.( + )" =
  (module struct
    open Value

    let%expect_test "( + )" =
      Size (10., Px) + Size (2., Px) |> show |> print_endline;
      [%expect {| (Size (12., Px)) |}]

    let%expect_test "( + )" =
      Size (10., Px) + Keyword "auto" |> show |> print_endline;
      [%expect {| (Size (10., Px)) |}]
  end)

let%test_module "Value.build" =
  (module struct
    open Value

    let%expect_test "build" =
      [ "12px" ] |> build |> show |> print_endline;
      [%expect {| (Size (12., Px)) |}]

    let%expect_test "build value of a hex color code" =
      [ "#"; "bb22FF" ] |> build |> show |> print_endline;
      [%expect {| (Rgb (187, 34, 255)) |}]

    let%expect_test "build" =
      [ "inline" ] |> build |> show |> print_endline;
      [%expect {| (Keyword "inline") |}]
  end)
