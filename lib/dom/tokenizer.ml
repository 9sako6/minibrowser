exception Unknown_character of string

let print_tokens ?(separator = ",") tokens =
  print_endline (String.concat separator tokens)

let%expect_test "print_tokens with '|' separator" =
  print_tokens [ "<"; "div"; ">"; "a"; "<"; "/"; "div"; ">" ] ~separator:"|";
  [%expect {| <|div|>|a|<|/|div|> |}]

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

let tokenize input_string =
  let rec acc tokens chars =
    match chars with
    | [] -> (tokens, [])
    | ' ' :: rest | '\n' :: rest -> acc tokens rest
    | '<' :: rest | '>' :: rest | '/' :: rest | '=' :: rest | '"' :: rest ->
        let token = List.hd chars |> Base.String.of_char in
        acc (tokens @ [ token ]) rest
    | _ ->
        let chunk, rest = tokenize_chunk chars in
        acc (tokens @ [ chunk ]) rest
  in
  let tokens, _ = acc [] (Base.String.to_list input_string) in
  tokens

let%expect_test "tokenize a tag" =
  "<div>hello</div>" |> tokenize |> print_tokens;
  [%expect {| <,div,>,hello,<,/,div,> |}]

let%expect_test "tokenize a tag with number text" =
  "<div>bob2</div>" |> tokenize |> print_tokens;
  [%expect {| <,div,>,bob2,<,/,div,> |}]

let%expect_test "tokenize a tag containing a hyphen in the name" =
  "<my-element></my-element>" |> tokenize |> print_tokens;
  [%expect {| <,my-element,>,<,/,my-element,> |}]

let%expect_test "tokenize a tag that has a class attribute" =
  "<div class=\"container\">hello</div>" |> tokenize |> print_tokens;
  [%expect {| <,div,class,=,",container,",>,hello,<,/,div,> |}]

let%expect_test "tokenize a tag that has children" =
  "<ul>\n<li>alice</li>\n<li>bob</li></ul>" |> tokenize |> print_tokens;
  [%expect {| <,ul,>,<,li,>,alice,<,/,li,>,<,li,>,bob,<,/,li,>,<,/,ul,> |}]
