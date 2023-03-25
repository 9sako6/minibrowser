exception Not_matched of string

let text_re = Re.Posix.compile_pat "[^<]+"
let attribute_value_re = Re.Posix.compile_pat "[^\"]+"
let tag_re = Re.Posix.compile_pat "[A-Za-z0-9-]+"

let matched_string re str =
  try Re.Group.get (Re.exec re str) 0
  with Not_found -> raise @@ Not_matched str

let tokenize_with_re re chars =
  let input_string = Base.String.of_char_list chars in
  let chunk = matched_string re input_string in
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
          | true, _ -> tokenize_with_re text_re chars
          | false, true -> tokenize_with_re attribute_value_re chars
          | false, false -> tokenize_with_re tag_re chars
        in
        aux (tokens @ [ chunk ]) rest
  in
  let tokens, _ = aux [] (Base.String.to_list input_string) in
  tokens
