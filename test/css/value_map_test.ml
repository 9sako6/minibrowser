open Css

let%expect_test "lookup" =
  let value = Value.Size (12., Px) in
  Value_map.empty
  |> Value_map.add "padding" value
  |> Value_map.lookup [ "padding-left"; "padding" ] (Value.Keyword "default")
  |> Value.show |> print_endline;
  [%expect {| (Size (12., Px)) |}]

let%expect_test "lookup sets default value if a value doesn't exist" =
  Value_map.empty
  |> Value_map.lookup [ "padding-left"; "padding" ] (Value.Keyword "default")
  |> Value.show |> print_endline;
  [%expect {| (Keyword "default") |}]
