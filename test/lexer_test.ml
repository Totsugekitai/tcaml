open OUnit2
open Tcaml_lib
open Lexer

let arithmetic _ =
  assert_equal [ IntLit 3 ] (lexer "3");
  assert_equal [ IntLit 325 ] (lexer "325");
  assert_equal [ IntLit 3; Plus; IntLit 2 ] (lexer "3+2");
  assert_equal [ IntLit 3; Minus; IntLit 2 ] (lexer "3 - 2")

let identifier _ =
  assert_equal [ LowerIdent "foo" ] (lexer "foo");
  assert_equal [ LowerIdent "foo"; LowerIdent "bar" ] (lexer "foo bar");
  assert_equal
    [ CapitalIdent "FooBar"; CapitalIdent "Baz" ]
    (lexer "FooBar Baz");
  assert_equal
    [ CapitalIdent "FooBar_"; CapitalIdent "Baz" ]
    (lexer "FooBar_ Baz")

let reserved_word _ =
  assert_equal [ Let ] (lexer "let");
  assert_equal [ LowerIdent "let's"; LowerIdent "play" ] (lexer "let's play");
  assert_equal
    [ LowerIdent "let's"; LowerIdent "play"; Let ]
    (lexer "let's play let");
  assert_equal
    [ In; Match; With; Of; If; Then; Else ]
    (lexer "in match with of if then else")

let symbol _ =
  assert_equal [ At; Dot; Comma; Quote; DoubleQuote ] (lexer "@ . , ' \"")

let comment _ =
  assert_equal [] (lexer "(* aaa *)");
  assert_equal [] (lexer "(* aaa (* bbb *) *)");
  assert_equal [ LowerIdent "ddd" ] (lexer "(* aaa (* bbb(*ccc*) *) *) ddd")
