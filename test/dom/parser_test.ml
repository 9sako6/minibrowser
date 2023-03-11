let%test_module "parse" =
  (module struct
    open Dom.Tokenizer
    open Dom.Parser

    let () = Printexc.record_backtrace false

    let%expect_test "parse no end tag" =
      ignore (tokenize "<div>" |> parse);
      [%expect.unreachable]
      [@@expect.uncaught_exn {| (Dom.Parser.NoEndTag) |}]

    let%expect_test "parse no end tag with child" =
      ignore (tokenize "<div><p>child</p>" |> parse);
      [%expect.unreachable]
      [@@expect.uncaught_exn {| (Dom.Parser.NoEndTag) |}]

    let%expect_test "parse no end tag bracket" =
      ignore (tokenize "<div" |> parse);
      [%expect.unreachable]
      [@@expect.uncaught_exn {| (Dom.Parser.NoEndTag) |}]

    let%expect_test "parse 1 emtpy tag" =
      tokenize "<div></div>" |> parse |> List.map Dom.Node.show
      |> List.iter print_endline;
      [%expect {|
        (Element ("div", [], [])) |}]

    let%expect_test "parse 2 tags" =
      tokenize "<div>alice</div><div>bob</div>"
      |> parse |> List.map Dom.Node.show |> List.iter print_endline;
      [%expect
        {|
        (Element ("div", [], [(InnerText "alice")]))
        (Element ("div", [], [(InnerText "bob")])) |}]

    let%expect_test "parse with child" =
      tokenize "<div>alice<p>child</p></div>"
      |> parse |> List.map Dom.Node.show |> List.iter print_endline;
      [%expect
        {|
        (Element ("div", [],
           [(InnerText "alice"); (Element ("p", [], [(InnerText "child")]))])) |}]

    let%expect_test "parse with child and other" =
      tokenize "<div>alice<p>child</p></div><div>bob</div>"
      |> parse |> List.map Dom.Node.show |> List.iter print_endline;
      [%expect
        {|
        (Element ("div", [],
           [(InnerText "alice"); (Element ("p", [], [(InnerText "child")]))]))
        (Element ("div", [], [(InnerText "bob")])) |}]

    let%expect_test "parse text" =
      tokenize "hello" |> parse |> List.map Dom.Node.show
      |> List.iter print_endline;
      [%expect {| (InnerText "hello") |}]

    let%expect_test "parse div tag with attribute" =
      tokenize "<div class=\"red\">hi</div>"
      |> parse |> List.map Dom.Node.show |> List.iter print_endline;
      [%expect
        {|
          (Element ("div", [("class", "red")], [(InnerText "hi")]))
        |}]

    let%expect_test "parse div tag with newline" =
      tokenize "\n  <div class=\"red\">\n    hi\n  </div>"
      |> parse |> List.map Dom.Node.show |> List.iter print_endline;
      [%expect
        {|
          (Element ("div", [("class", "red")], [(InnerText "hi")]))
        |}]

    let%expect_test "parse div tag with attribute and children" =
      tokenize "<div class=\"red\">\n  hi<p>a</p><p>b</p></div>"
      |> parse |> List.map Dom.Node.show |> List.iter print_endline;
      [%expect
        {|
          (Element ("div", [("class", "red")],
             [(InnerText "hi"); (Element ("p", [], [(InnerText "a")]));
               (Element ("p", [], [(InnerText "b")]))]
             ))
        |}]
  end)
