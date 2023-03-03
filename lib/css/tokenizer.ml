let tokenize_chunk chars =
  let chunk_regexp = Str.regexp "[A-Za-z]+" in
  let input_string = String_util.chars_to_string chars in
  let chunk =
    if Str.string_match chunk_regexp input_string 0 then
      Str.matched_string input_string
    else failwith (Printf.sprintf "Unknown character `%c`." input_string.[0])
  in
  let pos = String.length chunk in
  let _, rest = List_util.split pos chars in
  (chunk, rest)

let%expect_test "tokenize_chunk" =
  let chunk, rest = tokenize_chunk [ 'f'; 'o'; 'o'; '{'; '}' ] in
  print_endline chunk;
  assert (rest = [ '{'; '}' ]);
  [%expect {| foo |}]

let tokenize input_string =
  let rec acc chars =
    match chars with
    | [] -> []
    | ' ' :: rest -> acc rest
    | '.' :: rest | '{' :: rest | '}' :: rest | ';' :: rest | ':' :: rest ->
        String.make 1 (List.hd chars) :: acc rest
    | _ ->
        let chunk, rest = tokenize_chunk chars in
        chunk :: acc rest
  in
  acc (String_util.split input_string)

let print_tokens tokens = print_endline (String.concat "," tokens)

let%expect_test "tokenize" =
  tokenize ".foo { display: none; }" |> print_tokens;
  [%expect {|.,foo,{,display,:,none,;,}|}]
