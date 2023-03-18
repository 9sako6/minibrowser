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
