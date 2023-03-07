exception Invalid_size_value of string
exception Invalid_value of string

type size_unit = Px

type t =
  | Keyword of string
  | Size of float * size_unit
  | Rgb of int * int * int

let to_string = function
  | Keyword keyword -> keyword
  | Size (size, unit) -> (
      match unit with
      | Px -> Printf.sprintf "%s px" (string_of_float size))
  | Rgb (r, g, b) -> Printf.sprintf "rgb(%d, %d, %d)" r g b

let%expect_test "to_string" =
  Rgb (100, 0, 1) |> to_string |> print_endline;
  [%expect {| rgb(100, 0, 1) |}]

let get_size_value value =
  match value with
  | Size (size, _) -> size
  | Keyword "auto" -> 0.
  | _ -> Invalid_size_value (to_string value) |> raise

let ( + ) left right =
  match (left, right) with
  | Size (left_size, _), Size (right_size, _) ->
      Size (left_size +. right_size, Px)
  | Size (size, _), Keyword "auto" | Keyword "auto", Size (size, _) ->
      Size (size, Px)
  | Keyword "auto", Keyword "auto" -> Size (0., Px)
  | _ ->
      let error_message =
        Printf.sprintf "left: %s, right: %s" (to_string left) (to_string right)
      in
      raise (Invalid_size_value error_message)

let%expect_test "( + )" =
  Size (10., Px) + Size (2., Px) |> to_string |> print_endline;
  [%expect {| 12. px |}]

let%expect_test "( + )" =
  Size (10., Px) + Keyword "auto" |> to_string |> print_endline;
  [%expect {| 10. px |}]

let rgb_of_hex hex_string =
  let r =
    Printf.sprintf "0x%c%c" hex_string.[0] hex_string.[1] |> int_of_string
  in
  let g =
    Printf.sprintf "0x%c%c" hex_string.[2] hex_string.[3] |> int_of_string
  in
  let b =
    Printf.sprintf "0x%c%c" hex_string.[4] hex_string.[5] |> int_of_string
  in
  Rgb (r, g, b)

let%expect_test "rgb_of_hex" =
  "bb2254" |> rgb_of_hex |> to_string |> print_endline;
  [%expect {| rgb(187, 34, 84) |}]

let build tokens =
  let px_regexp = Str.regexp "[0-9]+px" in
  let number_regexp = Str.regexp "[0-9]+" in
  match tokens with
  | "#" :: [ hex_string ] -> rgb_of_hex hex_string
  | head :: _ -> (
      match
        ( Str.string_match px_regexp head 0,
          Str.string_match number_regexp head 0 )
      with
      | true, true ->
          let size = head |> Str.matched_string |> float_of_string in
          Size (size, Px)
      | _ -> Keyword (String.concat "" tokens))
  | [] -> Invalid_value (String.concat "" tokens) |> raise

let%expect_test "build" =
  [ "12px" ] |> build |> to_string |> print_endline;
  [%expect {| 12. px |}]

let%expect_test "build" =
  [ "#"; "191919" ] |> build |> to_string |> print_endline;
  [%expect {| rgb(25, 25, 25) |}]

let%expect_test "build" =
  [ "inline" ] |> build |> to_string |> print_endline;
  [%expect {| inline |}]
