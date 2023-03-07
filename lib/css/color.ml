type t = int * int * int

let string_of_rgb = function
  | r, g, b -> Printf.sprintf "(%d, %d, %d)" r g b

let rgb_of_hex hex_string =
  Scanf.sscanf hex_string "%2x%2x%2x" (fun r g b -> (r, g, b))

let%expect_test "rgb_of_hex" =
  "bb2254" |> rgb_of_hex |> string_of_rgb |> print_endline;
  [%expect {| (187, 34, 84) |}]

let%expect_test "rgb_of_hex" =
  "FF2254" |> rgb_of_hex |> string_of_rgb |> print_endline;
  [%expect {| (255, 34, 84) |}]
