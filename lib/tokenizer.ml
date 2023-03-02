type token =
  | OpenTag
  | CloseTag
  | Slash
  | Equal
  | DoubleQuote
  | Text of string

let to_string = function
  | OpenTag -> "<"
  | CloseTag -> ">"
  | Slash -> "/"
  | Equal -> "="
  | DoubleQuote -> "\""
  | Text text -> text

let print_tokens tokens =
  List.iter print_string (List.map to_string tokens);
  print_endline ""

let%expect_test "print_tokens 1 tag" =
  print_tokens
    [
      OpenTag;
      Text "div";
      CloseTag;
      Text "a";
      OpenTag;
      Slash;
      Text "div";
      CloseTag;
    ];
  [%expect {| <div>a</div> |}]

let get_text_value str =
  let text_regexp = Str.regexp "[A-Za-z]+" in
  if Str.string_match text_regexp str 0 then Str.matched_string str
  else failwith (Printf.sprintf "Unknown character `%c`." str.[0])

let%test "get_text_value \"div>\"" = get_text_value "div>" = "div"
let%test "get_text_value \"div\"" = get_text_value "div" = "div"

let tokenize input_str =
  let rec acc chars =
    match chars with
    | [] -> []
    | ' ' :: rest -> acc rest
    | '<' :: rest -> OpenTag :: acc rest
    | '>' :: rest -> CloseTag :: acc rest
    | '/' :: rest -> Slash :: acc rest
    | '=' :: rest -> Equal :: acc rest
    | '"' :: rest -> DoubleQuote :: acc rest
    | _ ->
        let sub_string = String_util.chars_to_string chars in
        let text_value = get_text_value sub_string in
        let text_value_length = String.length text_value in
        let rest_sub_string =
          String.sub sub_string text_value_length
            (String.length sub_string - text_value_length)
        in
        Text text_value :: acc (String_util.split rest_sub_string)
  in
  acc (String_util.split input_str)

let%test "tokenize <div>a</div>" =
  tokenize "<div>a</div>"
  = [
      OpenTag;
      Text "div";
      CloseTag;
      Text "a";
      OpenTag;
      Slash;
      Text "div";
      CloseTag;
    ]

let%test "tokenize <div class=\"container\">a</div>" =
  tokenize "<div class=\"container\">a</div>"
  = [
      OpenTag;
      Text "div";
      Text "class";
      Equal;
      DoubleQuote;
      Text "container";
      DoubleQuote;
      CloseTag;
      Text "a";
      OpenTag;
      Slash;
      Text "div";
      CloseTag;
    ]
