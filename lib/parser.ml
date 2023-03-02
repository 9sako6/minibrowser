open Tokenizer

type node =
  | Element of string * token list * node list
  | InnerText of string

let children_and_rest_tokans tokens =
  let rec split score children tails =
    match tails with
    | [] -> failwith "Fail to parse."
    | OpenTag :: Slash :: rest ->
        split (score - 1) (children @ [ OpenTag; Slash ]) rest
    | OpenTag :: rest -> split (score + 1) (children @ [ OpenTag ]) rest
    | head :: rest ->
        if score = 0 then
          match head with
          | CloseTag -> (children @ [ CloseTag ], rest)
          | Text _ -> split 0 (children @ [ head ]) rest
          | _ -> failwith "Fail to parse."
        else if score > 0 then split score (children @ [ head ]) rest
        else failwith "Fail to parse."
  in
  let children, rest = split 1 [] tokens in
  let children =
    match List_util.reverse children with
    | CloseTag :: Text _ :: Slash :: OpenTag :: tails -> List_util.reverse tails
    | _ -> failwith "There is no close tag."
  in
  (children, rest)

let%test "split_children_and_rest <p>child1</p><p>child2</p></p><p>rest</p>" =
  children_and_rest_tokans
    [
      (* 親の開始タグ <p> 以降が入力される *)
      (* <p>child1</p><p>child2</p> *)
      OpenTag;
      Text "p";
      CloseTag;
      Text "child1";
      OpenTag;
      Slash;
      Text "p";
      CloseTag;
      OpenTag;
      Text "p";
      CloseTag;
      Text "child2";
      OpenTag;
      Slash;
      Text "p";
      CloseTag;
      OpenTag;
      Slash;
      Text "p";
      CloseTag;
      (* <p>rest</p> *)
      OpenTag;
      Text "p";
      CloseTag;
      Text "rest";
      OpenTag;
      Slash;
      Text "p";
      CloseTag;
    ]
  = ( [
        OpenTag;
        Text "p";
        CloseTag;
        Text "child1";
        OpenTag;
        Slash;
        Text "p";
        CloseTag;
        OpenTag;
        Text "p";
        CloseTag;
        Text "child2";
        OpenTag;
        Slash;
        Text "p";
        CloseTag;
      ],
      [
        OpenTag;
        Text "p";
        CloseTag;
        Text "rest";
        OpenTag;
        Slash;
        Text "p";
        CloseTag;
      ] )

let to_string node =
  let attributes_to_string attributes =
    List.map Tokenizer.to_string attributes |> String_util.join
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
        Printf.sprintf "%s<%s %s>\n%s" prefix name attrs
          (nodes_to_string (prefix ^ " ") children)
    | InnerText text -> Printf.sprintf "%s↳%s\n" prefix text
  in
  node_to_string "" node

let%expect_test "to_string div tag with child" =
  Element
    ("div", [], [ InnerText "alice"; Element ("p", [], [ InnerText "child" ]) ])
  |> to_string |> print_endline;
  [%expect {|
  <div >
   ↳alice
   <p >
    ↳child
  |}]

let create_attribute tokens = tokens

let attributes_and_rest_tokens tokens =
  let rec split attributes rest =
    match rest with
    | [] -> (attributes, [])
    | CloseTag :: rest -> (attributes, rest)
    | Text attribute_name
      :: Equal :: DoubleQuote
      :: Text attribute_value
      :: DoubleQuote :: rest ->
        let attribute =
          create_attribute
            [
              Text attribute_name;
              Equal;
              DoubleQuote;
              Text attribute_value;
              DoubleQuote;
            ]
        in
        split (attributes @ attribute) rest
    | _ -> ([], rest)
  in
  split [] tokens

let rec parse tokens =
  match tokens with
  | [] -> []
  (* Start of tag *)
  | OpenTag :: Text tag_name :: rest ->
      let attributes, rest = attributes_and_rest_tokens rest in
      let children, rest = children_and_rest_tokans rest in
      Element (tag_name, attributes, parse children) :: parse rest
  (* End of tag *)
  | Text text :: OpenTag :: Slash :: Text _name :: CloseTag :: rest ->
      InnerText text :: parse rest
  | Text text :: rest -> InnerText text :: parse rest
  | _ ->
      failwith
        (Printf.sprintf "Fail to parse: %s"
           (String.concat "," (List.map Tokenizer.to_string tokens)))

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
  <div class="red">
   ↳hi
  |}]

let%expect_test "parse div tag with attribute and children" =
  tokenize "<div class=\"red\">hi<p>a</p><p>b</p></div>"
  |> parse |> List.hd |> to_string |> print_endline;
  [%expect {|
  <div class="red">
   ↳hi
   <p >
    ↳a
   <p >
    ↳b
  |}]
