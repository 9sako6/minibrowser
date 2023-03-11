open Css.Tokenizer

let%expect_test "tokenize" =
  tokenize ".foo { display: none; }" |> print_tokens;
  [%expect {|. foo { display : none ; }|}]
