let split str =
  let rec _split i chars =
    if i < 0 then chars else _split (i - 1) (str.[i] :: chars)
  in
  _split (String.length str - 1) []

let%test "split string into chars" = split "hello" = [ 'h'; 'e'; 'l'; 'l'; 'o' ]
let%test "split empty string into chars" = split "" = []

let join chars =
  let result = Bytes.create (List.length chars) in
  let rec _join i = function
    | [] -> result
    | c :: l ->
        Bytes.set result i c;
        _join (i + 1) l
  in
  Bytes.to_string (_join 0 chars)

let%test "join chars" = join [ 'h'; 'e'; 'l'; 'l'; 'o' ] = "hello"
let%test "join empty chars" = join [] = ""
