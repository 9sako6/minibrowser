let%test_module "parse" =
  (module struct
    let () = Printexc.record_backtrace false

    let%expect_test "parse no end tag" =
      ignore (Dom.Tokenizer.tokenize "<div>" |> Dom.Parser.parse);
      [%expect.unreachable]
      [@@expect.uncaught_exn {| (Dom.Parser.NoEndTag) |}]

    let%expect_test "parse no end tag with child" =
      ignore (Dom.Tokenizer.tokenize "<div><p>child</p>" |> Dom.Parser.parse);
      [%expect.unreachable]
      [@@expect.uncaught_exn {| (Dom.Parser.NoEndTag) |}]

    let%expect_test "parse no end tag bracket" =
      ignore (Dom.Tokenizer.tokenize "<div" |> Dom.Parser.parse);
      [%expect.unreachable]
      [@@expect.uncaught_exn {| (Dom.Parser.NoEndTag) |}]
  end)
