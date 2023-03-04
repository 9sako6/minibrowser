let split str =
  let rec _split i chars =
    if i < 0 then chars else _split (i - 1) (str.[i] :: chars)
  in
  _split (String.length str - 1) []

let%test "split string into chars" = split "hello" = [ 'h'; 'e'; 'l'; 'l'; 'o' ]
let%test "split empty string into chars" = split "" = []

let chars_to_string chars =
  let result = Bytes.create (List.length chars) in
  let rec _join i = function
    | [] -> result
    | c :: l ->
        Bytes.set result i c;
        _join (i + 1) l
  in
  Bytes.to_string (_join 0 chars)

let%expect_test "chars_to_string chars" =
  chars_to_string [ 'h'; 'e'; 'l'; 'l'; 'o' ] |> print_endline;
  [%expect {| hello |}]

let%expect_test "chars_to_string empty chars" =
  chars_to_string [] |> print_endline;
  [%expect {| |}]
