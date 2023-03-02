(* let test_transpile_error invalid_program expected_error () =
  let run () =
    try failwith "" with _ -> Alcotest.fail "No exception was thrown"
    (* with e -> raise e *)
  in
  Alcotest.check_raises "test_transpile_error" expected_error run *)
