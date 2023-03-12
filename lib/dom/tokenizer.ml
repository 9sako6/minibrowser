exception Not_matched of string

let matched_string regexp string =
  if Str.string_match regexp string 0 then Str.matched_string string
  else Not_matched string |> raise

let tokenize_text chars =
  let regexp = Str.regexp "[^<]+" in
  let input_string = Base.String.of_char_list chars in
  let chunk = matched_string regexp input_string in
  let rest =
    Base.String.drop_prefix input_string (String.length chunk)
    |> Base.String.to_list
  in
  (chunk, rest)

let tokenize_attribute_value chars =
  let regexp = Str.regexp "[^\"]+" in
  let input_string = Base.String.of_char_list chars in
  let chunk = matched_string regexp input_string in
  let rest =
    Base.String.drop_prefix input_string (String.length chunk)
    |> Base.String.to_list
  in
  (chunk, rest)

let tokenize_chunk chars =
  let regexp = Str.regexp "[A-Za-z0-9-]+" in
  let input_string = Base.String.of_char_list chars in
  let chunk = matched_string regexp input_string in
  let rest =
    Base.String.drop_prefix input_string (String.length chunk)
    |> Base.String.to_list
  in
  (chunk, rest)

let tokenize input_string =
  let rec aux ?(is_in_tag = false) ?(is_in_attribute = false) tokens chars =
    match chars with
    | [] -> (tokens, [])
    | (' ' | '\n') :: rest -> aux tokens rest
    | (('<' | '/' | '=') as head) :: rest ->
        aux (tokens @ [ Base.String.of_char head ]) rest ~is_in_tag:false
    | '"' :: rest ->
        aux (tokens @ [ "\"" ]) rest ~is_in_tag:false
          ~is_in_attribute:(not is_in_attribute)
    | '>' :: rest -> aux (tokens @ [ ">" ]) rest ~is_in_tag:true
    | _ ->
        let chunk, rest =
          match (is_in_tag, is_in_attribute) with
          | true, _ -> tokenize_text chars
          | false, true -> tokenize_attribute_value chars
          | false, false -> tokenize_chunk chars
        in
        aux (tokens @ [ chunk ]) rest
  in
  let tokens, _ = aux [] (Base.String.to_list input_string) in
  tokens
