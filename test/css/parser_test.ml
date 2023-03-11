let%test_module "parse" =
  (module struct
    open Css.Parser
    open Css.Node
    open Css.Tokenizer

    let%expect_test "parse a universal selector" =
      "* {font-size: 14px;}" |> tokenize |> parse |> show_stylesheet
      |> print_endline;
      [%expect
        {|
          (Stylesheet
             [(Rule ([Universal_selector],
                 [(Declaration ("font-size", (Size (14., Px))))]))
               ])
        |}]

    let%expect_test "parse a class selector" =
      ".alert {color: red;}" |> tokenize |> parse |> show_stylesheet
      |> print_endline;
      [%expect
        {|
          (Stylesheet
             [(Rule ([(Class_selector "alert")],
                 [(Declaration ("color", (Keyword "red")))]))
               ])
        |}]

    let%expect_test "parse a RGB value" =
      ".alert {background-color: #aaBB99;}" |> tokenize |> parse
      |> show_stylesheet |> print_endline;
      [%expect
        {|
          (Stylesheet
             [(Rule ([(Class_selector "alert")],
                 [(Declaration ("background-color", (Rgb (170, 187, 153))))]))
               ])
        |}]

    let%expect_test "parse empty declaration" =
      ".foo {}" |> tokenize |> parse |> show_stylesheet |> print_endline;
      [%expect {| (Stylesheet [(Rule ([(Class_selector "foo")], []))]) |}]

    let%expect_test "parse multiple declarations" =
      ".foo {\n  display: none;\n  color: #191919;\n  font-size: 14px;\n}\n"
      |> tokenize |> parse |> show_stylesheet |> print_endline;
      [%expect
        {|
          (Stylesheet
             [(Rule ([(Class_selector "foo")],
                 [(Declaration ("display", (Keyword "none")));
                   (Declaration ("color", (Rgb (25, 25, 25))));
                   (Declaration ("font-size", (Size (14., Px))))]
                 ))
               ])
        |}]

    let%expect_test "parse multiple class selectors" =
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
