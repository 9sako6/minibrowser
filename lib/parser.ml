open Tokenizer

type node =
  | Tag of string * node list
  | TextTag of string

let split_children_and_rest tokens =
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
  split_children_and_rest
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
  let rec nodes_to_string prefix nodes =
    match nodes with
    | [] -> ""
    | head :: rest ->
        Printf.sprintf "%s%s"
          (node_to_string prefix head)
          (nodes_to_string prefix rest)
  and node_to_string prefix = function
    | Tag (name, children) ->
        Printf.sprintf "%s<%s>\n%s" prefix name
          (nodes_to_string (prefix ^ " ") children)
    | TextTag text -> Printf.sprintf "%s↳%s\n" prefix text
  in
  node_to_string "" node

let%expect_test "to_string div tag" =
  Tag ("div", [ TextTag "alice"; Tag ("p", [ TextTag "child" ]) ])
  |> to_string |> print_endline;
  [%expect {|
  <div>
   ↳alice
   <p>
    ↳child
  |}]

let rec parse tokens =
  match tokens with
  | [] -> []
  | OpenTag :: Text name :: CloseTag :: rest ->
      let children, rest = split_children_and_rest rest in
      Tag (name, parse children) :: parse rest
  | Text text :: OpenTag :: Slash :: Text _name :: CloseTag :: rest ->
      TextTag text :: parse rest
  | Text text :: rest -> TextTag text :: parse rest
  | _ ->
      failwith
        (Printf.sprintf "Fail to parse: %s"
           (String.concat "," (List.map Tokenizer.to_string tokens)))

let%test "parse 1 tag" =
  tokenize "<div>alice</div>" |> parse = [ Tag ("div", [ TextTag "alice" ]) ]

let%test "parse 1 emtpy tag" =
  tokenize "<div></div>" |> parse = [ Tag ("div", []) ]

let%test "parse 2 tags" =
  tokenize "<div>alice</div><div>bob</div>"
  |> parse
  = [ Tag ("div", [ TextTag "alice" ]); Tag ("div", [ TextTag "bob" ]) ]

let%test "parse with child" =
  tokenize "<div>alice<p>child</p></div>"
  |> parse
  = [ Tag ("div", [ TextTag "alice"; Tag ("p", [ TextTag "child" ]) ]) ]

let%test "parse with child and other" =
  tokenize "<div>alice<p>child</p></div><div>bob</div>"
  |> parse
  = [
      Tag ("div", [ TextTag "alice"; Tag ("p", [ TextTag "child" ]) ]);
      Tag ("div", [ TextTag "bob" ]);
    ]
