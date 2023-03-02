let reverse list =
  let rec rev reversed rest =
    match rest with
    | [] -> reversed
    | head :: rest -> rev (head :: reversed) rest
  in
  rev [] list

let%test "reverse string list" = reverse [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ]
let%test "reverse int list" = reverse [ 1; 2; 3 ] = [ 3; 2; 1 ]
