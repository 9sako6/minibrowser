type t = {
  node : Dom.Node.t ref;
  specified_values : Value_map.t;
  children : t list;
}

let rec string_of_style ?(indent = "") = function
  | { node = dom_node; specified_values = map; children } ->
      let dom_string = Dom.Node.string_of_node !dom_node in
      let map_string = Value_map.to_string map in
      let children_string =
        children
        |> List.map (string_of_style ~indent:(indent ^ "  "))
        |> String.concat ""
      in
      Printf.sprintf "%s--\n%sStyle\n%s%s\n%s%s\n%s" indent indent indent
        dom_string indent map_string children_string

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

let add_rule rule map =
  let declarations =
    match rule with
    | Css.Node.Rule (_, declarations) -> declarations
  in
  let rec acc map declarations =
    match declarations with
    | [] -> (map, [])
    | Css.Node.Declaration (name, value) :: rest ->
        acc (Value_map.add name value map) rest
  in
  let map, _ = acc map declarations in
  map

let add_rules rules map =
  let rec acc map rules =
    match rules with
    | [] -> (map, [])
    | rule :: rest -> acc (add_rule rule map) rest
  in
  let map, _ = acc map rules in
  map

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
  let map = Value_map.empty in
  let map = add_rules matched_rules map in
  { node = dom_node_ref; specified_values = map; children = style_children }

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
      color: tomato;
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
      font-size: 12. px;
        --
        Style
        InnerText("hello")
        font-size: 12. px;
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
      color: tomato; font-size: 12. px;
        --
        Style
        InnerText("hello")
        font-size: 12. px;
        --
        Style
        Element(p; [])
        font-size: 12. px;
          --
          Style
          InnerText("child")
          font-size: 12. px;
    |}]

let%expect_test "create node with conflicted CSS rules" =
  let dom_node_ref =
    "<div class=\"block\">hello</div>" |> Dom.Tokenizer.tokenize
    |> Dom.Parser.parse |> List.hd |> ref
  in
  let stylesheet =
    ".block {display: block;} * {display: inline;}" |> Css.Tokenizer.tokenize
    |> Css.Parser.parse
  in
  let style = create stylesheet dom_node_ref in
  style |> string_of_style |> print_endline;
  [%expect
    {|
      --
      Style
      Element(div; [class="block"])
      display: inline;
        --
        Style
        InnerText("hello")
        display: inline;
    |}]

module Value_map = Value_map
