exception Not_matched of string

let print_tokens ?(separator = ",") tokens =
  print_endline (String.concat separator tokens)

let%expect_test "print_tokens with '|' separator" =
  print_tokens [ "<"; "div"; ">"; "a"; "<"; "/"; "div"; ">" ] ~separator:"|";
  [%expect {| <|div|>|a|<|/|div|> |}]

let matched_string regexp string =
  if Str.string_match regexp string 0 then Str.matched_string string
  else Not_matched string |> raise

let tokenize_text chars =
  let text_regexp = Str.regexp "[^<]+" in
  let input_string = Base.String.of_char_list chars in
  let chunk = matched_string text_regexp input_string in
  let rest =
    Base.String.drop_prefix input_string (String.length chunk)
    |> Base.String.to_list
  in
  (chunk, rest)

let%expect_test "tokenize_text" =
  let text, rest =
    "Hello, World!</div>" |> Base.String.to_list |> tokenize_text
  in
  print_endline text;
  List.iter print_char rest;
  [%expect {|
    Hello, World!
    </div>
  |}]

let tokenize_chunk chars =
  let chunk_regexp = Str.regexp "[A-Za-z0-9-]+" in
  let input_string = Base.String.of_char_list chars in
  let chunk = matched_string chunk_regexp input_string in
  let rest =
    Base.String.drop_prefix input_string (String.length chunk)
    |> Base.String.to_list
  in
  (chunk, rest)

let%expect_test "tokenize_chunk" =
  let chunk, rest =
    "div>hello</div>" |> Base.String.to_list |> tokenize_chunk
  in
  print_endline chunk;
  List.iter print_char rest;
  [%expect {|
    div
    >hello</div>
  |}]

let tokenize input_string =
  let rec aux ?(is_in_tag = false) tokens chars =
    match chars with
    | [] -> (tokens, [])
    | ' ' :: rest | '\n' :: rest -> aux tokens rest
    | '<' :: rest | '/' :: rest | '=' :: rest | '"' :: rest ->
        let token = List.hd chars |> Base.String.of_char in
        aux (tokens @ [ token ]) rest ~is_in_tag:false
    | '>' :: rest -> aux (tokens @ [ ">" ]) rest ~is_in_tag:true
    | _ ->
        let chunk, rest =
          if is_in_tag then tokenize_text chars else tokenize_chunk chars
        in
        aux (tokens @ [ chunk ]) rest
  in
  let tokens, _ = aux [] (Base.String.to_list input_string) in
  tokens

let%expect_test "tokenize a tag" =
  "<div>hello</div>" |> tokenize |> print_tokens;
  [%expect {| <,div,>,hello,<,/,div,> |}]

let%expect_test "tokenize a tag with an attribute that has no value" =
  "<button disabled></button>" |> tokenize |> print_tokens;
  [%expect {| <,button,disabled,>,<,/,button,> |}]

let%expect_test "tokenize a tag with number text" =
  "<div>bob2</div>" |> tokenize |> print_tokens;
  [%expect {| <,div,>,bob2,<,/,div,> |}]

let%expect_test "tokenize a tag with a white space" =
  "<div>Hello Hello</div>" |> tokenize |> print_tokens;
  [%expect {| <,div,>,Hello Hello,<,/,div,> |}]

let%expect_test "tokenize a tag with a white space and signs" =
  "<div>Hello, World!</div>" |> tokenize |> print_tokens;
  [%expect {| <,div,>,Hello, World!,<,/,div,> |}]

let%expect_test "tokenize a tag containing a hyphen in the name" =
  "<my-element></my-element>" |> tokenize |> print_tokens;
  [%expect {| <,my-element,>,<,/,my-element,> |}]

let%expect_test "tokenize a tag that has a class attribute" =
  "<div class=\"container\">hello</div>" |> tokenize |> print_tokens;
  [%expect {| <,div,class,=,",container,",>,hello,<,/,div,> |}]

let%expect_test "tokenize a tag that has children" =
  "<ul>\n<li>alice</li>\n<li>bob</li></ul>" |> tokenize |> print_tokens;
  [%expect {| <,ul,>,<,li,>,alice,<,/,li,>,<,li,>,bob,<,/,li,>,<,/,ul,> |}]
