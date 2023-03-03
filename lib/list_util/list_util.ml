let reverse list =
  let rec rev reversed rest =
    match rest with
    | [] -> reversed
    | head :: rest -> rev (head :: reversed) rest
  in
  rev [] list

let%test "reverse string list" = reverse [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ]
let%test "reverse int list" = reverse [ 1; 2; 3 ] = [ 3; 2; 1 ]

let split pos list =
  let rec acc current left right =
    if current < pos then
      match right with
      | head :: rest -> acc (current + 1) (left @ [ head ]) rest
      | [] -> (left, [])
    else (left, right)
  in
  acc 0 [] list

let%expect_test "split list" =
  let left, right = split 2 [ 1; 2; 3; 4; 5 ] in
  left |> List.map string_of_int |> String.concat "," |> print_endline;
  right |> List.map string_of_int |> String.concat "," |> print_endline;
  [%expect {|
  1,2
  3,4,5
  |}]

let%expect_test "split list" =
  let left, right = split 2 [] in
  left |> List.map string_of_int |> String.concat "," |> print_endline;
  right |> List.map string_of_int |> String.concat "," |> print_endline;
  [%expect {| |}]
