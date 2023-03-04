type t = Style of Dom.Node.t ref * Css.Node.rule list * t list

let rec string_of_style ?(indent = "") = function
  | Style (dom_node, rules, style_children) ->
      let dom_string = Dom.Parser.string_of_node !dom_node in
      let rules_string =
        rules |> List.map Css.Parser.string_of_rule |> String.concat "; "
      in
      let style_children_str =
        style_children
        |> List.map (string_of_style ~indent:(indent ^ "  "))
        |> String.concat ""
      in
      Printf.sprintf "%s--\n%sStyle\n%s%s\n%s%s\n%s" indent indent indent
        dom_string indent rules_string style_children_str

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

let rec create stylesheet dom_node_ref =
  let matched_rules =
    match stylesheet with
    | Css.Node.Stylesheet rules -> List.filter (matches !dom_node_ref) rules
  in
  let node_children =
    match !dom_node_ref with
    | Element (_, _, children_nodes) -> children_nodes
    | InnerText _ -> []
  in
  let style_children =
    node_children |> List.map ref |> List.map (create stylesheet)
  in
  Style (dom_node_ref, matched_rules, style_children)

let%expect_test "create" =
  let dom_node_ref =
    "<div id=\"foo\" class=\"alert\">hello</div>" |> Dom.Tokenizer.tokenize
    |> Dom.Parser.parse |> List.hd |> ref
  in
  let stylesheet =
    ".alert {color: tomato;}" |> Css.Tokenizer.tokenize |> Css.Parser.parse
  in
  let style = create stylesheet dom_node_ref in
  style |> string_of_style |> print_endline;
  [%expect
    {|
      --
      Style
      Element(div; [id="foo"; class="alert"])
      Rule([Class_selector(alert)], [Declaration(color: tomato)])
        --
        Style
        InnerText("hello")
    |}]

let%expect_test "create" =
  let dom_node_ref =
    "<div id=\"foo\" class=\"alert\">hello</div>" |> Dom.Tokenizer.tokenize
    |> Dom.Parser.parse |> List.hd |> ref
  in
  let stylesheet =
    "* {font-size: 12px;}" |> Css.Tokenizer.tokenize |> Css.Parser.parse
  in
  let style = create stylesheet dom_node_ref in
  style |> string_of_style |> print_endline;
  [%expect
    {|
      --
      Style
      Element(div; [id="foo"; class="alert"])
      Rule([Universal_selector], [Declaration(font-size: 12px)])
        --
        Style
        InnerText("hello")
        Rule([Universal_selector], [Declaration(font-size: 12px)])
    |}]

let%expect_test "create" =
  let dom_node_ref =
    "<div id=\"foo\" class=\"alert\">hello<p>child</p></div>"
    |> Dom.Tokenizer.tokenize |> Dom.Parser.parse |> List.hd |> ref
  in
  let stylesheet =
    ".alert {color: tomato;} * {font-size: 12px;}" |> Css.Tokenizer.tokenize
    |> Css.Parser.parse
  in
  let style = create stylesheet dom_node_ref in
  style |> string_of_style |> print_endline;
  [%expect
    {|
      --
      Style
      Element(div; [id="foo"; class="alert"])
      Rule([Class_selector(alert)], [Declaration(color: tomato)]); Rule([Universal_selector], [Declaration(font-size: 12px)])
        --
        Style
        InnerText("hello")
        Rule([Universal_selector], [Declaration(font-size: 12px)])
        --
        Style
        Element(p; [])
        Rule([Universal_selector], [Declaration(font-size: 12px)])
          --
          Style
          InnerText("child")
          Rule([Universal_selector], [Declaration(font-size: 12px)])
    |}]
