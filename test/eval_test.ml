open OUnit2
open Tcaml_lib
open Value
open Expr

let literal _ =
  (* 整数リテラル *)
  assert_equal (IntVal 1) (eval (IntLit 1));
  assert_equal (IntVal 0) (eval (IntLit 0));
  assert_equal (IntVal (-1)) (eval (IntLit (-1)));
  (* 真偽値リテラル *)
  assert_equal (BoolVal true) (eval (BoolLit true));
  assert_equal (BoolVal false) (eval (BoolLit false))

let arithmetic _ =
  (* 加算 *)
  assert_equal (IntVal 6) (eval (Add (IntLit 2, IntLit 4)));
  assert_equal (IntVal 0) (eval (Add (IntLit 2, IntLit (-2))));
  assert_equal (IntVal (-2)) (eval (Add (IntLit 2, IntLit (-4))));
  (* 減算 *)
  assert_equal (IntVal 6) (eval (Sub (IntLit 10, IntLit 4)));
  assert_equal (IntVal 0) (eval (Sub (IntLit 10, IntLit 10)));
  assert_equal (IntVal 14) (eval (Sub (IntLit 10, IntLit (-4))));
  (* 乗算 *)
  assert_equal (IntVal 12) (eval (Mul (IntLit 3, IntLit 4)));
  assert_equal (IntVal (-12)) (eval (Mul (IntLit (-3), IntLit 4)));
  assert_equal (IntVal 0) (eval (Mul (IntLit 3, IntLit 0)));
  (* 除算 *)
  assert_equal (IntVal 4) (eval (Div (IntLit 12, IntLit 3)));
  assert_equal (IntVal 0) (eval (Div (IntLit 3, IntLit 12)));
  assert_raises (Failure "zero division") (fun () -> (eval (Div (IntLit 3, IntLit 0))));
  (* binopの例外検出 *)
  assert_raises (Failure "integer value expected") (fun () -> (eval (Add (IntLit 4, BoolLit true))));
  assert_raises (Failure "integer value expected") (fun () -> (eval (Sub (IntLit 4, BoolLit true))));
  assert_raises (Failure "integer value expected") (fun () -> (eval (Mul (IntLit 4, BoolLit true))));
  assert_raises (Failure "integer value expected") (fun () -> (eval (Div (IntLit 4, BoolLit true))))

let comp _ =
  (* Eq *)
  assert_equal (IntVal 3) (eval (IntLit 3));
  assert_equal (IntVal 0) (eval (IntLit 0));
  assert_equal (IntVal (-3)) (eval (IntLit (-3)));
  assert_equal (BoolVal true) (eval (BoolLit true));
  assert_equal (BoolVal false) (eval (BoolLit false));
