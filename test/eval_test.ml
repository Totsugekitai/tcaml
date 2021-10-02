open OUnit2
open Tcaml

let literal _ =
  (* 整数リテラル *)
  assert_equal (IntVal 1) (eval (IntLit 1) emptyenv);
  assert_equal (IntVal 0) (eval (IntLit 0) emptyenv);
  assert_equal (IntVal (-1)) (eval (IntLit (-1)) emptyenv);
  (* 真偽値リテラル *)
  assert_equal (BoolVal true) (eval (BoolLit true) emptyenv);
  assert_equal (BoolVal false) (eval (BoolLit false) emptyenv)

let arithmetic _ =
  (* 加算 *)
  assert_equal (IntVal 6) (eval (Add (IntLit 2, IntLit 4)) emptyenv);
  assert_equal (IntVal 0) (eval (Add (IntLit 2, IntLit (-2))) emptyenv);
  assert_equal (IntVal (-2)) (eval (Add (IntLit 2, IntLit (-4))) emptyenv);
  (* 減算 *)
  assert_equal (IntVal 6) (eval (Sub (IntLit 10, IntLit 4)) emptyenv);
  assert_equal (IntVal 0) (eval (Sub (IntLit 10, IntLit 10)) emptyenv);
  assert_equal (IntVal 14) (eval (Sub (IntLit 10, IntLit (-4))) emptyenv);
  (* 乗算 *)
  assert_equal (IntVal 12) (eval (Mul (IntLit 3, IntLit 4)) emptyenv);
  assert_equal (IntVal (-12)) (eval (Mul (IntLit (-3), IntLit 4)) emptyenv);
  assert_equal (IntVal 0) (eval (Mul (IntLit 3, IntLit 0)) emptyenv);
  (* 除算 *)
  assert_equal (IntVal 4) (eval (Div (IntLit 12, IntLit 3)) emptyenv);
  assert_equal (IntVal 0) (eval (Div (IntLit 3, IntLit 12)) emptyenv);
  assert_raises (Failure "zero division") (fun () ->
      eval (Div (IntLit 3, IntLit 0)) emptyenv);
  (* binopの例外検出 *)
  assert_raises (Failure "integer value expected") (fun () ->
      eval (Add (IntLit 4, BoolLit true)) emptyenv);
  assert_raises (Failure "integer value expected") (fun () ->
      eval (Sub (IntLit 4, BoolLit true)) emptyenv);
  assert_raises (Failure "integer value expected") (fun () ->
      eval (Mul (IntLit 4, BoolLit true)) emptyenv);
  assert_raises (Failure "integer value expected") (fun () ->
      eval (Div (IntLit 4, BoolLit true)) emptyenv)

let comp _ =
  (* Eq *)
  assert_equal (BoolVal true) (eval (Eq (IntLit 3, IntLit 3)) emptyenv);
  assert_equal (BoolVal false) (eval (Eq (IntLit 3, IntLit 4)) emptyenv);
  assert_equal (BoolVal true) (eval (Eq (BoolLit true, BoolLit true)) emptyenv);
  assert_equal (BoolVal true)
    (eval (Eq (BoolLit false, BoolLit false)) emptyenv);
  (* Greater *)
  assert_equal (BoolVal true) (eval (Greater (IntLit 5, IntLit 3)) emptyenv);
  assert_equal (BoolVal false) (eval (Greater (IntLit 3, IntLit 5)) emptyenv);
  assert_equal (BoolVal false) (eval (Greater (IntLit 3, IntLit 3)) emptyenv);
  (* Less *)
  assert_equal (BoolVal true) (eval (Less (IntLit 3, IntLit 5)) emptyenv);
  assert_equal (BoolVal false) (eval (Less (IntLit 5, IntLit 3)) emptyenv);
  assert_equal (BoolVal false) (eval (Less (IntLit 5, IntLit 5)) emptyenv)

let control_expr _ =
  (* If *)
  assert_equal (IntVal 3)
    (eval (If (BoolLit true, IntLit 3, IntLit 2)) emptyenv);
  assert_equal (IntVal 2)
    (eval (If (BoolLit false, IntLit 3, IntLit 2)) emptyenv)

let env _ =
  assert_raises (Failure "unbound variable: x") (fun () ->
      eval (Var "x") emptyenv);
  assert_raises (Failure "unbound variable: foo") (fun () ->
      eval (Var "foo") emptyenv);
  let env1 = ext emptyenv "x" (IntVal 3) in
  assert_equal (IntVal 3) (eval (Var "x") env1);
  assert_equal (IntVal 5)
    (eval
       (Let ("x", IntLit 3, Let ("y", IntLit 2, Add (Var "x", Var "y"))))
       env1);
  assert_equal (IntVal 2)
    (eval (Let ("x", IntLit 3, Let ("x", IntLit 2, Var "x"))) env1);
  assert_equal (IntVal 3)
    (eval
       (Let
          ( "x",
            IntLit 1,
            Let ("y", Add (Var "x", IntLit 1), Add (Var "x", Var "y")) ))
       emptyenv);
  assert_equal (IntVal 5)
    (eval
       (Let
          ( "x",
            IntLit 1,
            Add
              ( Let ("x", IntLit 2, Add (Var "x", IntLit 1)),
                Mul (Var "x", IntLit 2) ) ))
       emptyenv)
