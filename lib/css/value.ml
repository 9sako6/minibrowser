exception Invalid_size_value of string
exception Invalid_value of string

type size_unit = Px [@@deriving show]

type t =
  | Keyword of string
  | Size of float * size_unit
  | Rgb of int * int * int
[@@deriving show]

let get_size_value value =
  match value with
  | Size (size, _) -> size
  | Keyword "auto" -> 0.
  | _ -> Invalid_size_value (show value) |> raise

let ( + ) left right =
  match (left, right) with
  | Size (left_size, _), Size (right_size, _) ->
      Size (left_size +. right_size, Px)
  | Size (size, _), Keyword "auto" | Keyword "auto", Size (size, _) ->
      Size (size, Px)
  | Keyword "auto", Keyword "auto" -> Size (0., Px)
  | _ ->
      let error_message =
        Printf.sprintf "left: %s, right: %s" (show left) (show right)
      in
      raise (Invalid_size_value error_message)

let%expect_test "( + )" =
  Size (10., Px) + Size (2., Px) |> show |> print_endline;
  [%expect {| (Value.Size (12., Value.Px)) |}]

let%expect_test "( + )" =
  Size (10., Px) + Keyword "auto" |> show |> print_endline;
  [%expect {| (Value.Size (10., Value.Px)) |}]

let build tokens =
  let px_regexp = Str.regexp "[0-9]+px" in
  let number_regexp = Str.regexp "[0-9]+" in
  match tokens with
  | "#" :: [ hex_string ] ->
      let r, g, b = Color.rgb_of_hex hex_string in
      Rgb (r, g, b)
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
  [ "12px" ] |> build |> show |> print_endline;
  [%expect {| (Value.Size (12., Value.Px)) |}]

let%expect_test "build" =
  [ "#"; "191919" ] |> build |> show |> print_endline;
  [%expect {| (Value.Rgb (25, 25, 25)) |}]

let%expect_test "build" =
  [ "inline" ] |> build |> show |> print_endline;
  [%expect {| (Value.Keyword "inline") |}]
