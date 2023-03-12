exception Invalid_size_value of string
exception Invalid_value of string

type size_unit = Px [@@deriving show { with_path = false }]

type t =
  | Keyword of string
  | Size of float * size_unit
  | Rgb of int * int * int
[@@deriving show { with_path = false }]

let rgb_of_hex hex_string =
  Scanf.sscanf hex_string "%2x%2x%2x" (fun r g b -> (r, g, b))

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

let%test_module "( + )" =
  (module struct
    let%expect_test "( + )" =
      Size (10., Px) + Size (2., Px) |> show |> print_endline;
      [%expect {| (Size (12., Px)) |}]

    let%expect_test "( + )" =
      Size (10., Px) + Keyword "auto" |> show |> print_endline;
      [%expect {| (Size (10., Px)) |}]
  end)

let build tokens =
  let px_regexp = Str.regexp "[0-9]+px" in
  let number_regexp = Str.regexp "[0-9]+" in
  match tokens with
  | "#" :: [ hex_string ] ->
      let r, g, b = rgb_of_hex hex_string in
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

let%test_module "build" =
  (module struct
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
