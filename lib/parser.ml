open Tokenizer

type name = string

type node =
  | Tag of name * node list
  | Text of string

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
  split 1 [] tokens

let _ =
  let children, rest =
    split_children_and_rest
      [
        (* 親の開始タグ <p> 以降が入力される *)
        (* <p>child1</p><p>child2</p></p> *)
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
  in
  print_tokens children;
  print_tokens rest

let%test "split_children_and_rest <p>child1</p><p>child2</p></p><p>rest</p>" =
  split_children_and_rest
    [
      (* 親の開始タグ <p> 以降が入力される *)
      (* <p>child1</p><p>child2</p></p> *)
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

(* let parse tokens =
   match tokens with
   | OpenTag :: Text name :: CloseTag :: rest -> Tag (name, [])
   | _ *)
