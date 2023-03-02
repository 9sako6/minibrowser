let () =
  Alcotest.check_raises "parse no end tag" Html.Parser.NoEndTag
    (fun () ->
      let _ =
        Html.Tokenizer.tokenize "<div>" |> Html.Parser.parse
      in
      ())

let () =
  Alcotest.check_raises "parse no end tag with child"
    Html.Parser.NoEndTag (fun () ->
      let _ =
        Html.Tokenizer.tokenize "<div><p>child</p>"
        |> Html.Parser.parse
      in
      ())

let () =
  Alcotest.check_raises "parse no end tag bracket" Html.Parser.NoEndTag
    (fun () ->
      let _ =
        Html.Tokenizer.tokenize "<div" |> Html.Parser.parse
      in
      ())
