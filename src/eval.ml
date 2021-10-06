(* eval.ml *)

open Syntax
open Env

let rec eval e env =
  let binop f e1 e2 env =
    match (eval e1 env, eval e2 env) with
    | IntVal n1, IntVal n2 -> IntVal (f n1 n2)
    | _ -> failwith "integer value expected"
  in
  let rec is_same_list l1 l2 =
    if List.length l1 = List.length l2 then
      match (l1, l2) with
      | [], [] -> true
      | IntVal hd1 :: tl1, IntVal hd2 :: tl2 ->
          hd1 = hd2 && is_same_list tl1 tl2
      | BoolVal hd1 :: tl1, BoolVal hd2 :: tl2 ->
          hd1 = hd2 && is_same_list tl1 tl2
      | ListVal hd1 :: tl1, ListVal hd2 :: tl2 ->
          is_same_list hd1 hd2 && is_same_list tl1 tl2
      | _ -> false
    else false
  in
  match e with
  (* リテラル *)
  | IntLit n -> IntVal n
  | BoolLit b -> BoolVal b
  (* 算術演算 *)
  | Plus (e1, e2) -> binop ( + ) e1 e2 env
  | Minus (e1, e2) -> binop ( - ) e1 e2 env
  | Times (e1, e2) -> binop ( * ) e1 e2 env
  | Div (e1, e2) ->
      if eval e2 env = IntVal 0 then failwith "zero division"
      else binop ( / ) e1 e2 env
  (* 比較 *)
  | Eq (e1, e2) -> (
      match (eval e1 env, eval e2 env) with
      | IntVal n1, IntVal n2 -> BoolVal (n1 = n2)
      | BoolVal b1, BoolVal b2 -> BoolVal (b1 = b2)
      | ListVal l1, ListVal l2 -> BoolVal (is_same_list l1 l2)
      | _ -> failwith "wrong value")
  | InEq (e1, e2) -> (
      match (eval e1 env, eval e2 env) with
      | IntVal n1, IntVal n2 -> BoolVal (n1 <> n2)
      | BoolVal b1, BoolVal b2 -> BoolVal (b1 <> b2)
      | ListVal l1, ListVal l2 -> BoolVal (not (is_same_list l1 l2))
      | _ -> failwith "wrong value")
  | Greater (e1, e2) -> (
      match (eval e1 env, eval e2 env) with
      | IntVal n1, IntVal n2 -> BoolVal (n1 > n2)
      | _ -> failwith "wrong value")
  | Less (e1, e2) -> (
      match (eval e1 env, eval e2 env) with
      | IntVal n1, IntVal n2 -> BoolVal (n1 < n2)
      | _ -> failwith "wrong value")
  (* 制御構文 *)
  | If (e1, e2, e3) -> (
      match eval e1 env with
      | BoolVal true -> eval e2 env
      | BoolVal false -> eval e3 env
      | _ -> failwith "wrong value")
  (* 環境 *)
  | Var x -> lookup x env
  | Let (x, e1, e2) ->
      let env1 = ext env x (eval e1 env) in
      eval e2 env1
  (* 再帰関数 *)
  | LetRec (f, x, e1, e2) ->
      let env1 = ext env f (RecFunVal (f, x, e1, env)) in
      eval e2 env1
  (* 関数抽象 *)
  | Fun (x, e1) -> FunVal (x, e1, env)
  (* 関数適用 *)
  | App (e1, e2) -> (
      let funpart = eval e1 env in
      let arg = eval e2 env in
      match funpart with
      | FunVal (x, body, env1) ->
          let env2 = ext env1 x arg in
          eval body env2
      | RecFunVal (f, x, body, env1) ->
          let env2 = ext (ext env1 x arg) f funpart in
          eval body env2
      | _ -> failwith "wrong value in App")
  (* リストに対する操作 *)
  | Empty -> ListVal []
  | Cons (e1, e2) -> (
      match (eval e1 env, eval e2 env) with
      | v1, ListVal v2 -> ListVal (v1 :: v2)
      | _ -> failwith "unexpected list")
  | Head e -> (
      match eval e env with
      | ListVal [] -> ListVal []
      | ListVal (hd :: _) -> hd
      | _ -> failwith "expected list, but other found")
  | Tail e -> (
      match eval e env with
      | ListVal [] -> ListVal []
      | ListVal (_ :: tl) -> ListVal tl
      | _ -> failwith "expected list, but other found")
  | _ -> failwith "unexpected expr"
