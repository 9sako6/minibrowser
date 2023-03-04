let () =
  Alcotest.check_raises "parse no end tag" Dom.Parser.NoEndTag
    (fun () ->
      let _ =
        Dom.Tokenizer.tokenize "<div>" |> Dom.Parser.parse
      in
      ())

let () =
  Alcotest.check_raises "parse no end tag with child"
    Dom.Parser.NoEndTag (fun () ->
      let _ =
        Dom.Tokenizer.tokenize "<div><p>child</p>"
        |> Dom.Parser.parse
      in
      ())

let () =
  Alcotest.check_raises "parse no end tag bracket" Dom.Parser.NoEndTag
    (fun () ->
      let _ =
        Dom.Tokenizer.tokenize "<div" |> Dom.Parser.parse
      in
      ())
