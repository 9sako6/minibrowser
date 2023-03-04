type t = Style of Dom.Node.t ref * Css.Node.rule list * t list

let string_of_style = function
  | Style (dom_node, rules, _) ->
      let dom_string = Dom.Parser.to_string !dom_node in
      let rules_string =
        rules |> List.map Css.Parser.string_of_rule |> String.concat "; "
      in
      Printf.sprintf "Style\n--\n%s\n--\n%s" dom_string rules_string

(* any of the attributes match the selectors *)
let matches dom_node rule =
  let attributes =
    match dom_node with
    | Dom.Node.Element (_, attributes, _) -> attributes
    | Dom.Node.InnerText _ -> []
  in
  let selectors =
    match rule with
    | Css.Node.Rule (selectors, _) -> selectors
  in
  let matches_with_selector attributes selector =
    match selector with
    | Css.Node.Universal_selector -> true
    | Css.Node.Class_selector class_name ->
        let _matches attribute =
          let attribute_name = Dom.Node.Attribute.name attribute in
          let value = Dom.Node.Attribute.value attribute in
          attribute_name = "class" && value = class_name
        in
        List.exists _matches attributes
  in
  List.exists (matches_with_selector attributes) selectors

let%expect_test "matches" =
  let rule =
    Css.Node.Rule
      ([ Css.Node.Class_selector "alert"; Css.Node.Universal_selector ], [])
  in
  let dom_node =
    "<div class=\"alert\" id=\"foo\">alert</div>" |> Dom.Tokenizer.tokenize
    |> Dom.Parser.parse |> List.hd
  in
  matches dom_node rule |> string_of_bool |> print_endline;
  [%expect {| true |}]

let%expect_test "matches" =
  let rule = Css.Node.Rule ([ Css.Node.Universal_selector ], []) in
  let dom_node =
    "<div id=\"foo\">alert</div>" |> Dom.Tokenizer.tokenize |> Dom.Parser.parse
    |> List.hd
  in
  matches dom_node rule |> string_of_bool |> print_endline;
  [%expect {| true |}]

let%expect_test "matches" =
  let rule = Css.Node.Rule ([ Css.Node.Class_selector "alert" ], []) in
  let dom_node =
    "<div id=\"foo\">alert</div>" |> Dom.Tokenizer.tokenize |> Dom.Parser.parse
    |> List.hd
  in
  matches dom_node rule |> string_of_bool |> print_endline;
  [%expect {| false |}]

let create dom_node_ref stylesheet =
  let matched_rules =
    match stylesheet with
    | Css.Node.Stylesheet rules -> List.filter (matches !dom_node_ref) rules
  in
  Style (dom_node_ref, matched_rules, [])

let%expect_test "create" =
  let dom_node_ref =
    "<div id=\"foo\" class=\"alert\">hello</div>" |> Dom.Tokenizer.tokenize
    |> Dom.Parser.parse |> List.hd |> ref
  in
  let stylesheet =
    ".alert {color: tomato;}" |> Css.Tokenizer.tokenize |> Css.Parser.parse
  in
  let style = create dom_node_ref stylesheet in
  style |> string_of_style |> print_endline;
  [%expect
    {|
      Style
      --
      ↳div id="foo" class="alert"
        ↳#text: hello

      --
      Rule([Class_selector(alert)], [Declaration(color: tomato)])
    |}]

let%expect_test "create" =
  let dom_node_ref =
    "<div id=\"foo\" class=\"alert\">hello</div>" |> Dom.Tokenizer.tokenize
    |> Dom.Parser.parse |> List.hd |> ref
  in
  let stylesheet =
    "* {font-size: 12px;}" |> Css.Tokenizer.tokenize |> Css.Parser.parse
  in
  let style = create dom_node_ref stylesheet in
  style |> string_of_style |> print_endline;
  [%expect
    {|
      Style
      --
      ↳div id="foo" class="alert"
        ↳#text: hello

      --
      Rule([Universal_selector], [Declaration(font-size: 12px)])
    |}]
