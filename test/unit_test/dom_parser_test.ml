open Dom

let%test_module "parse" =
  (module struct
    let () = Printexc.record_backtrace false

    let%expect_test "parse no end tag" =
      ignore ("<div>" |> parse);
      [%expect.unreachable]
      [@@expect.uncaught_exn {| (Dom__Parser.NoEndTag) |}]

    let%expect_test "parse no end tag with child" =
      ignore ("<div><p>child</p>" |> parse);
      [%expect.unreachable]
      [@@expect.uncaught_exn {| (Dom__Parser.NoEndTag) |}]

    let%expect_test "parse no end tag bracket" =
      ignore ("<div" |> parse);
      [%expect.unreachable]
      [@@expect.uncaught_exn {| (Dom__Parser.NoEndTag) |}]

    let%expect_test "parse 1 emtpy tag" =
      "<div></div>" |> parse |> List.map Node.show |> List.iter print_endline;
      [%expect {|
        (Element ("div", [], [])) |}]

    let%expect_test "parse a tag containing a hyphen in the name" =
      "<my-element></my-element>" |> parse |> List.map Node.show
      |> List.iter print_endline;
      [%expect {|
        (Element ("my-element", [], [])) |}]

    let%expect_test "parse a tag with number text" =
      "<div>bob2</div>" |> parse |> List.map Node.show
      |> List.iter print_endline;
      [%expect {|
        (Element ("div", [], [(InnerText "bob2")])) |}]

    let%expect_test "parse a tag with a white space and signs" =
      "<div>Hello, World!</div>" |> parse |> List.map Node.show
      |> List.iter print_endline;
      [%expect
        {|
        (Element ("div", [], [(InnerText "Hello, World!")])) |}]

    let%expect_test "parse a div tag that has a class attribute" =
      {| <div class="red">hi</div> |} |> parse |> List.map Node.show
      |> List.iter print_endline;
      [%expect
        {|
          (Element ("div", [("class", "red")], [(InnerText "hi")]))
        |}]

    (* let%expect_test "parse a tag with an attribute that has no value" =
       "<button disabled></button>" |> parse |> List.map Node.show
       |> List.iter print_endline;
       [%expect
         {|
           (Element ("div", [("class", "red")], [(InnerText "hi")]))
         |}] *)

    let%expect_test "parse a link tag that has a css path as a href attribute" =
      {| <link href="global.css"></link> |} |> parse |> List.map Node.show
      |> List.iter print_endline;
      [%expect
        {|
          (Element ("link", [("href", "global.css")], []))
        |}]

    let%expect_test "parse div tag with newline" =
      "\n  <div class=\"red\">\n    hi\n  </div>" |> parse |> List.map Node.show
      |> List.iter print_endline;
      [%expect
        {|
          (Element ("div", [("class", "red")], [(InnerText "hi")]))
        |}]

    let%expect_test "parse 2 tags" =
      {|
        <div>alice</div>
        <div>bob</div>
      |} |> parse
      |> List.map Node.show |> List.iter print_endline;
      [%expect
        {|
        (Element ("div", [], [(InnerText "alice")]))
        (Element ("div", [], [(InnerText "bob")])) |}]

    let%expect_test "parse with child" =
      {|
        <div>
          alice
          <p>child</p>
        </div>
      |}
      |> parse |> List.map Node.show |> List.iter print_endline;
      [%expect
        {|
        (Element ("div", [],
           [(InnerText "alice"); (Element ("p", [], [(InnerText "child")]))])) |}]

    let%expect_test "parse with child and other" =
      {|
        <div>
          alice
          <p>child</p>
        </div>
        <div>bob</div>
      |}
      |> parse |> List.map Node.show |> List.iter print_endline;
      [%expect
        {|
        (Element ("div", [],
           [(InnerText "alice"); (Element ("p", [], [(InnerText "child")]))]))
        (Element ("div", [], [(InnerText "bob")])) |}]

    let%expect_test "parse text" =
      "hello" |> parse |> List.map Node.show |> List.iter print_endline;
      [%expect {| (InnerText "hello") |}]

    let%expect_test "parse div tag with attribute and children" =
      {|
        <div class="red">
          hi
          <p>a</p>
          <p>b</p>
        </div>
      |}
      |> parse |> List.map Node.show |> List.iter print_endline;
      [%expect
        {|
          (Element ("div", [("class", "red")],
             [(InnerText "hi"); (Element ("p", [], [(InnerText "a")]));
               (Element ("p", [], [(InnerText "b")]))]
             ))
        |}]
  end)
