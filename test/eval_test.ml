open OUnit2
open Lib
open Syntax
open Eval

let parse str = Parser.main Lexer.token (Lexing.from_string str)

let literal _ =
  (* 整数リテラル *)
  assert_equal (IntVal 1) (eval (parse "1") emptyenv);
  assert_equal (IntVal 0) (eval (parse "0") emptyenv);
  assert_equal (IntVal (-1)) (eval (parse "-1") emptyenv);
  (* 真偽値リテラル *)
  assert_equal (BoolVal true) (eval (parse "true") emptyenv);
  assert_equal (BoolVal false) (eval (parse "false") emptyenv)

let arithmetic _ =
  (* 加算 *)
  assert_equal (IntVal 6) (eval (parse "4 + 2") emptyenv);
  assert_equal (IntVal 0) (eval (parse "2 + (-2)") emptyenv);
  assert_equal (IntVal (-2)) (eval (parse "2+(-4)") emptyenv);
  (* 減算 *)
  assert_equal (IntVal 6) (eval (parse "10 - 4") emptyenv);
  assert_equal (IntVal 0) (eval (parse "10 - 10") emptyenv);
  assert_equal (IntVal 14) (eval (parse "10 - (-4)") emptyenv);
  (* 乗算 *)
  assert_equal (IntVal 12) (eval (parse "3 * 4") emptyenv);
  assert_equal (IntVal (-12)) (eval (parse "(-3) * 4") emptyenv);
  assert_equal (IntVal 0) (eval (parse "3 * 0") emptyenv);
  (* 除算 *)
  assert_equal (IntVal 4) (eval (parse "12 / 3") emptyenv);
  assert_equal (IntVal 0) (eval (parse "3 / 12") emptyenv);
  assert_raises (Failure "zero division") (fun () ->
      eval (parse "3 / 0") emptyenv);
  (* binopの例外検出 *)
  assert_raises (Failure "integer value expected") (fun () ->
      eval (parse "4 + true") emptyenv);
  assert_raises (Failure "integer value expected") (fun () ->
      eval (parse "4 - true") emptyenv);
  assert_raises (Failure "integer value expected") (fun () ->
      eval (parse "4 * true") emptyenv);
  assert_raises (Failure "integer value expected") (fun () ->
      eval (parse "4 / true") emptyenv)

let comp _ =
  (* Eq *)
  assert_equal (BoolVal true) (eval (parse " 3 = 3") emptyenv);
  assert_equal (BoolVal false) (eval (parse "3 = 4") emptyenv);
  assert_equal (BoolVal true) (eval (parse " true = true") emptyenv);
  assert_equal (BoolVal true) (eval (parse "false = false") emptyenv);
  (* Greater *)
  assert_equal (BoolVal true) (eval (parse "5 > 3") emptyenv);
  assert_equal (BoolVal false) (eval (parse "3 > 5") emptyenv);
  assert_equal (BoolVal false) (eval (parse "3 > 3") emptyenv);
  (* Less *)
  assert_equal (BoolVal true) (eval (parse "3 < 5") emptyenv);
  assert_equal (BoolVal false) (eval (parse "5 < 3") emptyenv);
  assert_equal (BoolVal false) (eval (parse "5 < 5") emptyenv)

let control_expr _ =
  (* If *)
  assert_equal (IntVal 3) (eval (parse "if true then 3 else 2") emptyenv);
  assert_equal (IntVal 2) (eval (parse "if false then 3 else 2") emptyenv)

let env _ =
  assert_raises (Failure "unbound variable: x") (fun () ->
      eval (parse "x") emptyenv);
  assert_raises (Failure "unbound variable: foo") (fun () ->
      eval (parse "foo") emptyenv);
  let env1 = ext emptyenv "x" (IntVal 3) in
  assert_equal (IntVal 3) (eval (parse "x") env1);
  assert_equal (IntVal 5) (eval (parse "let x = 3 in let y = 2 in x + y") env1);
  assert_equal (IntVal 2) (eval (parse "let x = 3 in let x = 2 in x") env1);
  assert_equal (IntVal 3)
    (eval (parse "let x = 1 in let y = x + 1 in x + y") emptyenv);
  assert_equal (IntVal 5)
    (eval (parse "let x = 1 in (let x = 2 in x + 1) + x * 2") emptyenv)
