open Dom.Tokenizer

let%test_module "print_tokens" =
  (module struct
    let%expect_test "print_tokens with '|' separator" =
      print_tokens [ "<"; "div"; ">"; "a"; "<"; "/"; "div"; ">" ] ~separator:"|";
      [%expect {| <|div|>|a|<|/|div|> |}]
  end)

let%test_module "tokenize" =
  (module struct
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

    let%expect_test "tokenize a link tag that has a css path as a href \
                     attribute" =
      "<link href=\"global.css\"></link>" |> tokenize |> print_tokens;
      [%expect {| <,link,href,=,",global.css,",>,<,/,link,> |}]
  end)
