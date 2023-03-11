open Css
open Css.Value_map

let%test_module "lookup" =
  (module struct
    let%expect_test "lookup" =
      let value = Value.Size (12., Px) in
      empty |> add "padding" value
      |> lookup [ "padding-left"; "padding" ] (Value.Keyword "default")
      |> Value.show |> print_endline;
      [%expect {| (Size (12., Px)) |}]

    let%expect_test "lookup sets default value if a value doesn't exist" =
      empty
      |> lookup [ "padding-left"; "padding" ] (Value.Keyword "default")
      |> Value.show |> print_endline;
      [%expect {| (Keyword "default") |}]
  end)
