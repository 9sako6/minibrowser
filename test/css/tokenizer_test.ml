open Css.Tokenizer

let%test_module "tokenize" =
  (module struct
    let%expect_test "tokenize" =
      tokenize ".foo { display: none; }" |> print_tokens;
      [%expect {|. foo { display : none ; }|}]
  end)
