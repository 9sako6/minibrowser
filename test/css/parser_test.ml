let%test_module "parse" =
  (module struct
    open Css.Parser
    open Css.Node
    open Css.Tokenizer

    let%expect_test "parse" =
      ".foo,.bar {\n  display: flex;\n  color: red;\n}\n" |> tokenize |> parse
      |> show_stylesheet |> print_endline;
      [%expect
        {|
          (Stylesheet
             [(Rule ([(Class_selector "foo"); (Class_selector "bar")],
                 [(Declaration ("display", (Keyword "flex")));
                   (Declaration ("color", (Keyword "red")))]
                 ))
               ])
        |}]
  end)
