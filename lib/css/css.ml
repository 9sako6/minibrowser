type t = Node.stylesheet

module Node = Node
module Value_map = Value_map
module Value = Value

let parse css = css |> Tokenizer.tokenize |> Parser.parse

let%test_module "parse" =
  (module struct
    let%expect_test "parse a universal selector" =
      "* {font-size: 14px;}" |> parse |> Node.show_stylesheet |> print_endline;
      [%expect
        {|
          (Stylesheet
             [(Rule ([Universal_selector],
                 [(Declaration ("font-size", (Size (14., Px))))]))
               ])
        |}]

    let%expect_test "parse a class selector" =
      ".alert {color: red;}" |> parse |> Node.show_stylesheet |> print_endline;
      [%expect
        {|
          (Stylesheet
             [(Rule ([(Class_selector "alert")],
                 [(Declaration ("color", (Keyword "red")))]))
               ])
        |}]

    let%expect_test "parse a RGB value" =
      ".alert {background-color: #aaBB99;}" |> parse |> Node.show_stylesheet
      |> print_endline;
      [%expect
        {|
          (Stylesheet
             [(Rule ([(Class_selector "alert")],
                 [(Declaration ("background-color", (Rgb (170, 187, 153))))]))
               ])
        |}]

    let%expect_test "parse empty declaration" =
      ".foo {}" |> parse |> Node.show_stylesheet |> print_endline;
      [%expect {| (Stylesheet [(Rule ([(Class_selector "foo")], []))]) |}]

    let%expect_test "parse multiple declarations" =
      ".foo {\n  display: none;\n  color: #191919;\n  font-size: 14px;\n}\n"
      |> parse |> Node.show_stylesheet |> print_endline;
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
      ".foo,.bar {\n  display: flex;\n  color: red;\n}\n" |> parse
      |> Node.show_stylesheet |> print_endline;
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
