let () =
  Alcotest.check_raises "parse no end tag" Minibrowser.Parser.NoEndTag
    (fun () ->
      let _ =
        Minibrowser.Tokenizer.tokenize "<div>" |> Minibrowser.Parser.parse
      in
      ())

let () =
  Alcotest.check_raises "parse no end tag with child"
    Minibrowser.Parser.NoEndTag (fun () ->
      let _ =
        Minibrowser.Tokenizer.tokenize "<div><p>child</p>"
        |> Minibrowser.Parser.parse
      in
      ())

let () =
  Alcotest.check_raises "parse no end tag bracket" Minibrowser.Parser.NoEndTag
    (fun () ->
      let _ =
        Minibrowser.Tokenizer.tokenize "<div" |> Minibrowser.Parser.parse
      in
      ())
