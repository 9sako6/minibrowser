type t = {
  node : Dom.Node.t ref;
  specified_values : Css.Value_map.t;
  children : t list;
}

let empty () =
  {
    node = ref Dom.Node.empty;
    specified_values = Css.Value_map.empty;
    children = [];
  }

let rec to_string ?(indent = "") = function
  | { node = dom_node; specified_values = map; children } ->
      let dom_string = Dom.Node.show !dom_node in
      let map_string = Css.Value_map.pp map in
      let children_string =
        children
        |> List.map (to_string ~indent:(indent ^ "  "))
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
  let rec aux map declarations =
    match declarations with
    | [] -> (map, [])
    | Css.Node.Declaration (name, value) :: rest ->
        aux (Css.Value_map.add name value map) rest
  in
  let map, _ = aux map declarations in
  map

let add_rules rules map =
  let rec aux map rules =
    match rules with
    | [] -> (map, [])
    | rule :: rest -> aux (add_rule rule map) rest
  in
  let map, _ = aux map rules in
  map

let rec build stylesheet dom_node_ref =
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
    node_children |> List.map ref |> List.map (build stylesheet)
  in
  let map = Css.Value_map.empty in
  let map = add_rules matched_rules map in
  { node = dom_node_ref; specified_values = map; children = style_children }

let build_styles ~html ~css =
  let dom_nodes = html |> Dom.Tokenizer.tokenize |> Dom.Parser.parse in
  let stylesheet = css |> Css.Tokenizer.tokenize |> Css.Parser.parse in
  dom_nodes |> List.map ref |> List.map (build stylesheet)

let get_background_color style =
  let default_color = (0, 0, 0) in
  try
    match Css.Value_map.find "background-color" style.specified_values with
    | Css.Value.Rgb (r, g, b) -> (r, g, b)
    | _ -> raise Not_found
  with Not_found -> default_color
