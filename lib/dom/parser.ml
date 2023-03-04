open Tokenizer
open Node

exception NoEndTag

let children_and_rest_tokans tokens =
  let rec split score children tails =
    match tails with
    | [] -> raise NoEndTag
    | "<" :: "/" :: rest -> split (score - 1) (children @ [ "<"; "/" ]) rest
    | "<" :: rest -> split (score + 1) (children @ [ "<" ]) rest
    | head :: rest ->
        if score = 0 then
          match head with
          | ">" -> (children @ [ ">" ], rest)
          | _ -> split 0 (children @ [ head ]) rest
        else if score > 0 then split score (children @ [ head ]) rest
        else failwith "Fail to parse."
  in
  let children, rest = split 1 [] tokens in
  let children =
    match List_util.reverse children with
    | ">" :: _ :: "/" :: "<" :: tails -> List_util.reverse tails
    | _ -> raise NoEndTag
  in
  (children, rest)

let%test "split_children_and_rest <p>child1</p><p>child2</p></p><p>rest</p>" =
  children_and_rest_tokans
    [
      (* <p>child1</p><p>child2</p> *)
      "<";
      "p";
      ">";
      "child1";
      "<";
      "/";
      "p";
      ">";
      "<";
      "p";
      ">";
      "child2";
      "<";
      "/";
      "p";
      ">";
      "<";
      "/";
      "p";
      ">";
      (* <p>rest</p> *)
      "<";
      "p";
      ">";
      "rest";
      "<";
      "/";
      "p";
      ">";
    ]
  = ( [
        "<";
        "p";
        ">";
        "child1";
        "<";
        "/";
        "p";
        ">";
        "<";
        "p";
        ">";
        "child2";
        "<";
        "/";
        "p";
        ">";
      ],
      [ "<"; "p"; ">"; "rest"; "<"; "/"; "p"; ">" ] )

let to_string node =
  let attributes_to_string attributes =
    attributes |> List.map Attribute.to_string |> String.concat " "
  in
  let rec nodes_to_string prefix nodes =
    match nodes with
    | [] -> ""
    | head :: rest ->
        Printf.sprintf "%s%s"
          (node_to_string prefix head)
          (nodes_to_string prefix rest)
  and node_to_string prefix = function
    | Element (name, attributes, children) ->
        let attrs = attributes_to_string attributes in
        Printf.sprintf "%s↳%s %s\n%s" prefix name attrs
          (nodes_to_string (prefix ^ "  ") children)
    | InnerText text -> Printf.sprintf "%s↳#text: %s\n" prefix text
  in
  node_to_string "" node

let%expect_test "to_string div tag with child" =
  Element
    ("div", [], [ InnerText "alice"; Element ("p", [], [ InnerText "child" ]) ])
  |> to_string |> print_endline;
  [%expect {|
  ↳div
    ↳#text: alice
    ↳p
      ↳#text: child
  |}]

let attributes_and_rest_tokens tokens =
  let rec split attributes rest =
    match rest with
    | [] -> (attributes, [])
    | ">" :: rest -> (attributes, rest)
    | attribute_name :: "=" :: "\"" :: attribute_value :: "\"" :: rest ->
        let attribute = Attribute.create attribute_name attribute_value in
        split (attributes @ [ attribute ]) rest
    | _ -> ([], rest)
  in
  split [] tokens

let rec parse tokens =
  match tokens with
  | [] -> []
  (* Start of tag *)
  | "<" :: tag_name :: rest ->
      let attributes, rest = attributes_and_rest_tokens rest in
      let children, rest = children_and_rest_tokans rest in
      Element (tag_name, attributes, parse children) :: parse rest
  (* End of tag *)
  | text :: "<" :: "/" :: _name :: ">" :: rest -> InnerText text :: parse rest
  | text :: rest -> InnerText text :: parse rest

let%test "parse 1 tag" =
  tokenize "<div>alice</div>"
  |> parse
  = [ Element ("div", [], [ InnerText "alice" ]) ]

let%test "parse 1 emtpy tag" =
  tokenize "<div></div>" |> parse = [ Element ("div", [], []) ]

let%test "parse 2 tags" =
  tokenize "<div>alice</div><div>bob</div>"
  |> parse
  = [
      Element ("div", [], [ InnerText "alice" ]);
      Element ("div", [], [ InnerText "bob" ]);
    ]

let%test "parse with child" =
  tokenize "<div>alice<p>child</p></div>"
  |> parse
  = [
      Element
        ( "div",
          [],
          [ InnerText "alice"; Element ("p", [], [ InnerText "child" ]) ] );
    ]

let%test "parse with child and other" =
  tokenize "<div>alice<p>child</p></div><div>bob</div>"
  |> parse
  = [
      Element
        ( "div",
          [],
          [ InnerText "alice"; Element ("p", [], [ InnerText "child" ]) ] );
      Element ("div", [], [ InnerText "bob" ]);
    ]

let%test "parse text" = tokenize "hello" |> parse = [ InnerText "hello" ]

let%expect_test "parse div tag with attribute" =
  tokenize "<div class=\"red\">hi</div>"
  |> parse |> List.hd |> to_string |> print_endline;
  [%expect {|
  ↳div class="red"
    ↳#text: hi
  |}]

let%expect_test "parse div tag with newline" =
  tokenize "\n  <div class=\"red\">\n    hi\n  </div>"
  |> parse |> List.hd |> to_string |> print_endline;
  [%expect {|
  ↳div class="red"
    ↳#text: hi
  |}]

let%expect_test "parse div tag with attribute and children" =
  tokenize "<div class=\"red\">\n  hi<p>a</p><p>b</p></div>"
  |> parse |> List.hd |> to_string |> print_endline;
  [%expect
    {|
  ↳div class="red"
    ↳#text: hi
    ↳p
      ↳#text: a
    ↳p
      ↳#text: b
  |}]
