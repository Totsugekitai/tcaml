open OUnit2

let suite =
  "TestList" >::: [
    "Eval_test.literal" >:: Eval_test.literal;
    "Eval_test.arithmetic" >:: Eval_test.arithmetic;
    "Eval_test.comp" >:: Eval_test.comp
  ]

let () = run_test_tt_main suite