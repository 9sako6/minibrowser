exception Unknown_character of string

let tokenize_chunk chars =
  let chunk_regexp = Str.regexp "[A-Za-z0-9-]+" in
  let input_string =
    chars |> List.map Base.String.of_char |> String.concat ""
  in
  let chunk =
    if Str.string_match chunk_regexp input_string 0 then
      Str.matched_string input_string
    else raise (Unknown_character (Base.String.of_char input_string.[0]))
  in
  let pos = String.length chunk in
  let _, rest = Base.List.split_n chars pos in
  (chunk, rest)

let%expect_test "tokenize_chunk" =
  let chunk, rest = tokenize_chunk [ 'f'; 'o'; 'o'; '{'; '}' ] in
  print_endline chunk;
  assert (rest = [ '{'; '}' ]);
  [%expect {| foo |}]

let tokenize input_string =
  let rec aux tokens chars =
    match chars with
    | [] -> (tokens, [])
    | ' ' :: rest | '\n' :: rest -> aux tokens rest
    | '.' :: rest
    | '{' :: rest
    | '}' :: rest
    | ';' :: rest
    | ':' :: rest
    | '#' :: rest
    | '*' :: rest
    | ',' :: rest -> aux (tokens @ [ Base.String.of_char (List.hd chars) ]) rest
    | _ ->
        let chunk, rest = tokenize_chunk chars in
        aux (tokens @ [ chunk ]) rest
  in
  let tokens, _ = aux [] (Base.String.to_list input_string) in
  tokens

let print_tokens tokens = print_endline (String.concat " " tokens)

let%expect_test "tokenize" =
  tokenize ".foo { display: none; }" |> print_tokens;
  [%expect {|. foo { display : none ; }|}]
