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

let tokenize_attribute_value chars =
  let regexp = Str.regexp "[^\"]+" in
  let input_string = Base.String.of_char_list chars in
  let chunk = matched_string regexp input_string in
  let rest =
    Base.String.drop_prefix input_string (String.length chunk)
    |> Base.String.to_list
  in
  (chunk, rest)

let%expect_test "tokenize_attribute_value" =
  let text, rest =
    "alert\">Hello, World!</div>" |> Base.String.to_list
    |> tokenize_attribute_value
  in
  print_endline text;
  List.iter print_char rest;
  [%expect {|
    alert
    ">Hello, World!</div>
  |}]

let tokenize_chunk chars =
  let regexp = Str.regexp "[A-Za-z0-9-]+" in
  let input_string = Base.String.of_char_list chars in
  let chunk = matched_string regexp input_string in
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

let%test_module "tokenize" =
  (module struct
    let%expect_test "tokenize a tag" =
      "<div>hello</div>" |> tokenize |> [%derive.show: string list]
      |> print_endline;
      [%expect {| ["<"; "div"; ">"; "hello"; "<"; "/"; "div"; ">"] |}]

    let%expect_test "tokenize a tag with an attribute that has no value" =
      "<button disabled></button>" |> tokenize |> [%derive.show: string list]
      |> print_endline;
      [%expect {| ["<"; "button"; "disabled"; ">"; "<"; "/"; "button"; ">"] |}]

    let%expect_test "tokenize a tag with number text" =
      "<div>bob2</div>" |> tokenize |> [%derive.show: string list]
      |> print_endline;
      [%expect {| ["<"; "div"; ">"; "bob2"; "<"; "/"; "div"; ">"] |}]

    let%expect_test "tokenize a tag with a white space" =
      "<div>Hello Hello</div>" |> tokenize |> [%derive.show: string list]
      |> print_endline;
      [%expect {| ["<"; "div"; ">"; "Hello Hello"; "<"; "/"; "div"; ">"] |}]

    let%expect_test "tokenize a tag with a white space and signs" =
      "<div>Hello, World!</div>" |> tokenize |> [%derive.show: string list]
      |> print_endline;
      [%expect {| ["<"; "div"; ">"; "Hello, World!"; "<"; "/"; "div"; ">"] |}]

    let%expect_test "tokenize a tag containing a hyphen in the name" =
      "<my-element></my-element>" |> tokenize |> [%derive.show: string list]
      |> print_endline;
      [%expect {| ["<"; "my-element"; ">"; "<"; "/"; "my-element"; ">"] |}]

    let%expect_test "tokenize a tag that has a class attribute" =
      "<div class=\"container\">hello</div>" |> tokenize
      |> [%derive.show: string list] |> print_endline;
      [%expect {|
        ["<"; "div"; "class"; "="; "\""; "container"; "\""; ">"; "hello"; "<"; "/";
          "div"; ">"] |}]

    let%expect_test "tokenize a tag that has children" =
      "<ul>\n<li>alice</li>\n<li>bob</li></ul>" |> tokenize
      |> [%derive.show: string list] |> print_endline;
      [%expect {|
        ["<"; "ul"; ">"; "<"; "li"; ">"; "alice"; "<"; "/"; "li"; ">"; "<"; "li";
          ">"; "bob"; "<"; "/"; "li"; ">"; "<"; "/"; "ul"; ">"] |}]

    let%expect_test "tokenize a link tag that has a css path as a href \
                     attribute" =
      "<link href=\"global.css\"></link>" |> tokenize
      |> [%derive.show: string list] |> print_endline;
      [%expect {|
        ["<"; "link"; "href"; "="; "\""; "global.css"; "\""; ">"; "<"; "/"; "link";
          ">"] |}]
  end)
