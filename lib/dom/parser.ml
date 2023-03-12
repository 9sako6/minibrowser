open Tokenizer
open Node

exception NoEndTag

let parse_children_tokens tokens =
  let omit_end_tag_tokens tokens =
    match Base.List.rev tokens with
    | ">" :: _ :: "/" :: "<" :: rest -> Base.List.rev rest
    | _ -> raise NoEndTag
  in
  (* When the score is positive, some tag is open. *)
  let rec aux score children tails =
    match tails with
    | [] -> raise NoEndTag
    (* Each time a tag is closed, the score is decremented. *)
    | "<" :: "/" :: rest -> aux (score - 1) (children @ [ "<"; "/" ]) rest
    (* Each time a tag is opened, the score is incremented. *)
    | "<" :: rest -> aux (score + 1) (children @ [ "<" ]) rest
    | head :: rest -> (
        match (head, score) with
        | ">", 0 ->
            (* Omit parent's end tag. *)
            let children_tokens = children @ [ ">" ] |> omit_end_tag_tokens in
            (children_tokens, rest)
        | _, 0 -> aux 0 (children @ [ head ]) rest
        | _ ->
            if score > 0 then aux score (children @ [ head ]) rest
            else raise NoEndTag)
  in

  let children_tokens, rest = aux 1 [] tokens in
  (children_tokens, rest)

let%expect_test "parse_children_tokens" =
  let children_tokens, rest =
    "<p>child1</p><p>child2</p></p><p>rest</p>" |> tokenize
    |> parse_children_tokens
  in
  children_tokens |> [%derive.show: string list] |> print_endline;
  rest |> [%derive.show: string list] |> print_endline;
  [%expect
    {|
    ["<"; "p"; ">"; "child1"; "<"; "/"; "p"; ">"; "<"; "p"; ">"; "child2"; "<";
      "/"; "p"; ">"]
    ["<"; "p"; ">"; "rest"; "<"; "/"; "p"; ">"]
  |}]

let rec parse tokens =
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
  in

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
