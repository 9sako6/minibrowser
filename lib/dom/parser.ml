open Tokenizer
open Node

exception NoEndTag

let omit_end_tag_tokens tokens =
  match Base.List.rev tokens with
  | ">" :: _ :: "/" :: "<" :: rest -> Base.List.rev rest
  | _ -> raise NoEndTag

let%expect_test "omit_end_tag_tokens" =
  "<li>bob</li></div>" |> tokenize |> omit_end_tag_tokens |> print_tokens;
  [%expect {| <,li,>,bob,<,/,li,> |}]

let parse_children_tokens tokens =
  (* When the score is positive, some tag is open. *)
  let rec split score children tails =
    match tails with
    | [] -> raise NoEndTag
    (* Each time a tag is closed, the score is decremented. *)
    | "<" :: "/" :: rest -> split (score - 1) (children @ [ "<"; "/" ]) rest
    (* Each time a tag is opened, the score is incremented. *)
    | "<" :: rest -> split (score + 1) (children @ [ "<" ]) rest
    | head :: rest -> (
        match (head, score) with
        | ">", 0 ->
            (* Omit parent's end tag. *)
            let children_tokens = children @ [ ">" ] |> omit_end_tag_tokens in
            (children_tokens, rest)
        | _, 0 -> split 0 (children @ [ head ]) rest
        | _ ->
            if score > 0 then split score (children @ [ head ]) rest
            else raise NoEndTag)
  in

  let children_tokens, rest = split 1 [] tokens in
  (children_tokens, rest)

let%expect_test "parse_children_tokens" =
  let children_tokens, rest =
    "<p>child1</p><p>child2</p></p><p>rest</p>" |> tokenize
    |> parse_children_tokens
  in
  children_tokens |> print_tokens;
  rest |> print_tokens;
  [%expect
    {|
    <,p,>,child1,<,/,p,>,<,p,>,child2,<,/,p,>
    <,p,>,rest,<,/,p,>
  |}]

let parse_attributes tokens =
  let rec aux attributes rest =
    match rest with
    | [] -> (attributes, [])
    | ">" :: rest -> (attributes, rest)
    | name :: "=" :: "\"" :: value :: "\"" :: rest ->
        let attribute = Attribute.build name value in
        aux (attributes @ [ attribute ]) rest
    | _ -> ([], rest)
  in
  aux [] tokens

let rec parse tokens =
  let rec aux nodes rest =
    match rest with
    | [] -> (nodes, [])
    (* Start of a tag *)
    | "<" :: tag_name :: rest ->
        let attributes, rest = parse_attributes rest in
        let children_tokens, rest = parse_children_tokens rest in
        aux
          (nodes @ [ Element (tag_name, attributes, parse children_tokens) ])
          rest
    (* End of a tag *)
    | text :: "<" :: "/" :: _ :: ">" :: rest
    (* Inner text *)
    | text :: rest -> aux (nodes @ [ InnerText text ]) rest
  in
  let nodes, _ = aux [] tokens in
  nodes

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
