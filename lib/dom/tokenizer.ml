let print_tokens tokens = print_endline (String.concat "," tokens)

let%expect_test "print_tokens 1 tag" =
  print_tokens [ "<"; "div"; ">"; "a"; "<"; "/"; "div"; ">" ];
  [%expect {| <,div,>,a,<,/,div,> |}]

let get_text_value str =
  let text_regexp = Str.regexp "[A-Za-z0-9]+" in
  if Str.string_match text_regexp str 0 then Str.matched_string str
  else failwith (Printf.sprintf "Unknown character `%c`." str.[0])

let%test "get_text_value \"div>\"" = get_text_value "div>" = "div"
let%test "get_text_value \"div\"" = get_text_value "div" = "div"

let%expect_test "get_text_value with number" =
  get_text_value "55go</div>" |> print_endline;
  [%expect {| 55go |}]

let tokenize input_str =
  let rec acc tokens chars =
    match chars with
    | [] -> (tokens, [])
    | ' ' :: rest | '\n' :: rest -> acc tokens rest
    | '<' :: rest | '>' :: rest | '/' :: rest | '=' :: rest | '"' :: rest ->
        let token = List.hd chars |> String.make 1 in
        acc (tokens @ [ token ]) rest
    | _ ->
        let sub_string = String_util.chars_to_string chars in
        let token = get_text_value sub_string in
        let pos = String.length token in
        let _, rest = List_util.split pos chars in
        acc (tokens @ [ token ]) rest
  in
  let tokens, _ = acc [] (String_util.split input_str) in
  tokens

let%test "tokenize <div>a</div>" =
  tokenize "<div>a</div>" = [ "<"; "div"; ">"; "a"; "<"; "/"; "div"; ">" ]

let%expect_test "tokenize tag with number text" =
  tokenize "<div>bob2</div>" |> print_tokens;
  [%expect {| <,div,>,bob2,<,/,div,> |}]

let%test "tokenize <div class=\"container\">a</div>" =
  tokenize "<div class=\"container\">a</div>"
  = [
      "<";
      "div";
      "class";
      "=";
      "\"";
      "container";
      "\"";
      ">";
      "a";
      "<";
      "/";
      "div";
      ">";
    ]
