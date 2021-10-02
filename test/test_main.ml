open OUnit2

let suite =
  "TestList"
  >::: [
         "Eval_test.literal" >:: Eval_test.literal;
         "Eval_test.arithmetic" >:: Eval_test.arithmetic;
         "Eval_test.comp" >:: Eval_test.comp;
         "Eval_test.control_expr" >:: Eval_test.control_expr;
         "Eval_test.env" >:: Eval_test.env;
       ]

let () = run_test_tt_main suite
