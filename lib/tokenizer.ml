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
  let rec acc chars =
    match chars with
    | [] -> []
    | ' ' :: rest | '\n' :: rest -> acc rest
    | '<' :: rest -> "<" :: acc rest
    | '>' :: rest -> ">" :: acc rest
    | '/' :: rest -> "/" :: acc rest
    | '=' :: rest -> "=" :: acc rest
    | '"' :: rest -> "\"" :: acc rest
    | _ ->
        let sub_string = String_util.chars_to_string chars in
        let text_value = get_text_value sub_string in
        let text_value_length = String.length text_value in
        let rest_sub_string =
          String.sub sub_string text_value_length
            (String.length sub_string - text_value_length)
        in
        text_value :: acc (String_util.split rest_sub_string)
  in
  acc (String_util.split input_str)

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
