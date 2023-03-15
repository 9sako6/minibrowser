type t = {
  node : Dom.Node.t ref;
  specified_values : Css.Value_map.t;
  children : t list;
  size : size;
}

and size = {
  width : px;
  height : px;
  padding : px;
  padding_top : px;
  padding_right : px;
  padding_bottom : px;
  padding_left : px;
  border : px;
  border_top : px;
  border_right : px;
  border_bottom : px;
  border_left : px;
  margin : px;
  margin_top : px;
  margin_right : px;
  margin_bottom : px;
  margin_left : px;
}

and px =
  | Px of float
  | Auto

let empty () =
  {
    node = ref Dom.Node.empty;
    specified_values = Css.Value_map.empty;
    children = [];
    size =
      {
        width = Auto;
        height = Auto;
        padding = Px 0.;
        padding_top = Px 0.;
        padding_right = Px 0.;
        padding_bottom = Px 0.;
        padding_left = Px 0.;
        border = Px 0.;
        border_top = Px 0.;
        border_right = Px 0.;
        border_bottom = Px 0.;
        border_left = Px 0.;
        margin = Px 0.;
        margin_top = Px 0.;
        margin_right = Px 0.;
        margin_bottom = Px 0.;
        margin_left = Px 0.;
      };
  }

let rec to_string ?(indent = "") = function
  | { node = dom_node; specified_values = map; children; size = _ } ->
      let dom_string = Dom.Node.show !dom_node in
      let map_string = Css.Value_map.show map in
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
    "<div class=\"alert\" id=\"foo\">alert</div>" |> Dom.parse |> List.hd
  in
  matches dom_node rule |> string_of_bool |> print_endline;
  [%expect {| true |}]

let%expect_test "matches" =
  let rule = Css.Node.Rule ([ Css.Node.Universal_selector ], []) in
  let dom_node = "<div id=\"foo\">alert</div>" |> Dom.parse |> List.hd in
  matches dom_node rule |> string_of_bool |> print_endline;
  [%expect {| true |}]

let%expect_test "matches" =
  let rule = Css.Node.Rule ([ Css.Node.Class_selector "alert" ], []) in
  let dom_node = "<div id=\"foo\">alert</div>" |> Dom.parse |> List.hd in
  matches dom_node rule |> string_of_bool |> print_endline;
  [%expect {| false |}]

let add_rule rule map =
  let declarations =
    match rule with
    | Css.Node.Rule (_, declarations) -> declarations
  in
  List.fold_left
    (fun acc_map (Css.Node.Declaration (name, value)) ->
      Css.Value_map.add name value acc_map)
    map declarations

let add_rules rules map =
  List.fold_left (fun acc_map rule -> add_rule rule acc_map) map rules

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
  let lookup_size_value css_property default_value map =
    Css.Value_map.lookup css_property (Css.Value.Size (default_value, Px)) map
    |> Css.Value.get_size_value
  in
  let width =
    try Px (Css.Value_map.find "width" map |> Css.Value.get_size_value)
    with Not_found -> Auto
  in
  let height =
    try Px (Css.Value_map.find "height" map |> Css.Value.get_size_value)
    with Not_found -> Auto
  in
  let padding = Px (lookup_size_value [ "padding" ] 0. map) in
  let padding_top =
    Px (lookup_size_value [ "padding-top"; "padding" ] 0. map)
  in
  let padding_right =
    Px (lookup_size_value [ "padding-right"; "padding" ] 0. map)
  in
  let padding_bottom =
    Px (lookup_size_value [ "padding-bottom"; "padding" ] 0. map)
  in
  let padding_left =
    Px (lookup_size_value [ "padding-left"; "padding" ] 0. map)
  in
  let border = Px (lookup_size_value [ "border" ] 0. map) in
  let border_top =
    Px (lookup_size_value [ "border-width-top"; "border" ] 0. map)
  in
  let border_right =
    Px (lookup_size_value [ "border-width-right"; "border" ] 0. map)
  in
  let border_bottom =
    Px (lookup_size_value [ "border-width-bottom"; "border" ] 0. map)
  in
  let border_left =
    Px (lookup_size_value [ "border-width-left"; "border" ] 0. map)
  in
  let margin = Px (lookup_size_value [ "margin" ] 0. map) in
  let margin_top = Px (lookup_size_value [ "margin-top"; "margin" ] 0. map) in
  let margin_right =
    Px (lookup_size_value [ "margin-right"; "margin" ] 0. map)
  in
  let margin_bottom =
    Px (lookup_size_value [ "margin-bottom"; "margin" ] 0. map)
  in
  let margin_left = Px (lookup_size_value [ "margin-left"; "margin" ] 0. map) in
  {
    node = dom_node_ref;
    specified_values = map;
    children = style_children;
    size =
      {
        width;
        height;
        padding;
        padding_top;
        padding_right;
        padding_bottom;
        padding_left;
        border;
        border_top;
        border_right;
        border_bottom;
        border_left;
        margin;
        margin_top;
        margin_right;
        margin_bottom;
        margin_left;
      };
  }

let get_background_color style =
  let default_color = (0, 0, 0) in
  try
    match Css.Value_map.find "background-color" style.specified_values with
    | Css.Value.Rgb (r, g, b) -> (r, g, b)
    | _ -> raise Not_found
  with Not_found -> default_color

let build_styles ~html ~css =
  let dom_nodes = html |> Dom.parse in
  let stylesheet = css |> Css.parse in
  dom_nodes |> List.map ref |> List.map (build stylesheet)

let%test_module "build_styles" =
  (module struct
    let%expect_test "build_styles" =
      let html = "<div id=\"foo\" class=\"alert\">hello</div>" in
      let css = ".alert {color: tomato;}" in
      let styles = build_styles ~css ~html in
      styles |> List.map to_string |> List.iter print_endline;
      [%expect
        {|
          --
          Style
          (Element ("div", [("id", "foo"); ("class", "alert")], [(InnerText "hello")]))
          [("color", (Keyword "tomato"))]
            --
            Style
            (InnerText "hello")
            []
        |}]

    let%expect_test "build_styles" =
      let html = "<div id=\"foo\" class=\"alert\">hello</div>" in
      let css = "* {font-size: 12px;}" in
      let styles = build_styles ~css ~html in
      styles |> List.map to_string |> List.iter print_endline;
      [%expect
        {|
          --
          Style
          (Element ("div", [("id", "foo"); ("class", "alert")], [(InnerText "hello")]))
          [("font-size", (Size (12., Px)))]
            --
            Style
            (InnerText "hello")
            [("font-size", (Size (12., Px)))]
        |}]

    let%expect_test "build_styles" =
      let html = "<div id=\"foo\" class=\"alert\">hello<p>child</p></div>" in
      let css = ".alert {color: tomato;} * {font-size: 12px;}" in
      let styles = build_styles ~css ~html in
      styles |> List.map to_string |> List.iter print_endline;
      [%expect
        {|
          --
          Style
          (Element ("div", [("id", "foo"); ("class", "alert")],
             [(InnerText "hello"); (Element ("p", [], [(InnerText "child")]))]))
          [("color", (Keyword "tomato")); ("font-size", (Size (12., Px)))]
            --
            Style
            (InnerText "hello")
            [("font-size", (Size (12., Px)))]
            --
            Style
            (Element ("p", [], [(InnerText "child")]))
            [("font-size", (Size (12., Px)))]
              --
              Style
              (InnerText "child")
              [("font-size", (Size (12., Px)))]
        |}]

    let%expect_test "build node with conflicted CSS rules" =
      let html = "<div class=\"block\">hello</div>" in
      let css = ".block {display: block;} * {display: inline;}" in
      let styles = build_styles ~css ~html in
      styles |> List.map to_string |> List.iter print_endline;
      [%expect
        {|
          --
          Style
          (Element ("div", [("class", "block")], [(InnerText "hello")]))
          [("display", (Keyword "inline"))]
            --
            Style
            (InnerText "hello")
            [("display", (Keyword "inline"))]
        |}]
  end)

let get_size_value size =
  match size with
  | Px px -> px
  | Auto -> 0.

let ( + ) left right = Px (get_size_value left +. get_size_value right)
