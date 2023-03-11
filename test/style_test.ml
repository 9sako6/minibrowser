let%test_module "build_styles" =
  (module struct
    let%expect_test "build_styles" =
      let html = "<div id=\"foo\" class=\"alert\">hello</div>" in
      let css = ".alert {color: tomato;}" in
      let styles = Minibrowser.Style.build_styles ~css ~html in
      styles |> List.map Minibrowser.Style.to_string |> List.iter print_endline;
      [%expect
        {|
          --
          Style
          (Element ("div", [("id", "foo"); ("class", "alert")], [(InnerText "hello")]))
          color: (Keyword "tomato");
            --
            Style
            (InnerText "hello")
        |}]

    let%expect_test "build_styles" =
      let html = "<div id=\"foo\" class=\"alert\">hello</div>" in
      let css = "* {font-size: 12px;}" in
      let styles = Minibrowser.Style.build_styles ~css ~html in
      styles |> List.map Minibrowser.Style.to_string |> List.iter print_endline;
      [%expect
        {|
          --
          Style
          (Element ("div", [("id", "foo"); ("class", "alert")], [(InnerText "hello")]))
          font-size: (Size (12., Px));
            --
            Style
            (InnerText "hello")
            font-size: (Size (12., Px));
        |}]

    let%expect_test "build_styles" =
      let html = "<div id=\"foo\" class=\"alert\">hello<p>child</p></div>" in
      let css = ".alert {color: tomato;} * {font-size: 12px;}" in
      let styles = Minibrowser.Style.build_styles ~css ~html in
      styles |> List.map Minibrowser.Style.to_string |> List.iter print_endline;
      [%expect
        {|
          --
          Style
          (Element ("div", [("id", "foo"); ("class", "alert")],
             [(InnerText "hello"); (Element ("p", [], [(InnerText "child")]))]))
          color: (Keyword "tomato"); font-size: (Size (12., Px));
            --
            Style
            (InnerText "hello")
            font-size: (Size (12., Px));
            --
            Style
            (Element ("p", [], [(InnerText "child")]))
            font-size: (Size (12., Px));
              --
              Style
              (InnerText "child")
              font-size: (Size (12., Px));
        |}]

    let%expect_test "build node with conflicted CSS rules" =
      let html = "<div class=\"block\">hello</div>" in
      let css = ".block {display: block;} * {display: inline;}" in
      let styles = Minibrowser.Style.build_styles ~css ~html in
      styles |> List.map Minibrowser.Style.to_string |> List.iter print_endline;
      [%expect
        {|
          --
          Style
          (Element ("div", [("class", "block")], [(InnerText "hello")]))
          display: (Keyword "inline");
            --
            Style
            (InnerText "hello")
            display: (Keyword "inline");
        |}]
  end)
