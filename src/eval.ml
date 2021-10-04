(* eval.ml *)

open Syntax

let emptyenv = []

let ext env x v = (x, v) :: env

let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y, v) :: tl -> if x = y then v else lookup x tl

let rec is_same_list l1 l2 =
  if List.length l1 = List.length l2 then
    match (l1, l2) with
    | [], [] -> true
    | IntVal hd1 :: tl1, IntVal hd2 :: tl2 -> hd1 = hd2 && is_same_list tl1 tl2
    | BoolVal hd1 :: tl1, BoolVal hd2 :: tl2 ->
        hd1 = hd2 && is_same_list tl1 tl2
    | ListVal hd1 :: tl1, ListVal hd2 :: tl2 ->
        is_same_list hd1 hd2 && is_same_list tl1 tl2
    | _ -> false
  else false

let rec eval e env cont =
  let binop f e1 e2 env cont =
    match (eval e1 env cont, eval e2 env cont) with
    | IntVal n1, IntVal n2 -> IntVal (f n1 n2)
    | _ -> failwith "integer value expected"
  in
  match e with
  (* リテラル *)
  | IntLit n -> cont (IntVal n)
  | BoolLit b -> cont (BoolVal b)
  (* 算術演算 *)
  | Plus (e1, e2) -> binop ( + ) e1 e2 env cont
  | Minus (e1, e2) -> binop ( - ) e1 e2 env cont
  | Times (e1, e2) -> binop ( * ) e1 e2 env cont
  | Div (e1, e2) ->
      if eval e2 env cont = IntVal 0 then failwith "zero division"
      else binop ( / ) e1 e2 env cont
  (* 比較 *)
  | Eq (e1, e2) ->
      eval e1 env (fun v1 ->
          eval e2 env (fun v2 -> cont (BoolVal (eq_val v1 v2))))
  | InEq (e1, e2) ->
      eval e1 env (fun v1 ->
          eval e2 env (fun v2 -> cont (BoolVal (not (eq_val v1 v2)))))
  | Greater (e1, e2) ->
      eval e1 env (fun v1 ->
          eval e2 env (fun v2 -> cont (BoolVal (greater v1 v2))))
  | Less (e1, e2) ->
      eval e1 env (fun v1 ->
          eval e2 env (fun v2 -> cont (BoolVal (less v1 v2))))
  (* 制御構文 *)
  | If (e1, e2, e3) ->
      eval e1 env (function
        | BoolVal true -> eval e2 env cont
        | BoolVal false -> eval e3 env cont
        | _ -> failwith "wrong value")
  (* 環境 *)
  | Var x -> cont (lookup x env)
  | Let (x, e1, e2) ->
      eval e1 env (fun v1 ->
          let env1 = ext env x v1 in
          eval e2 env1 cont)
  (* 再帰関数 *)
  | LetRec (f, x, e1, e2) ->
      let env1 = ext env f (RecFunVal (f, x, e1, env)) in
      eval e2 env1 cont
  (* 関数抽象 *)
  | Fun (x, e1) -> cont (FunVal (x, e1, env))
  | App (e1, e2) ->
      eval e1 env (fun funpart -> eval e2 env (fun arg -> app funpart arg cont))
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

and eq_val v1 v2 =
  match (v1, v2) with
  | IntVal n1, IntVal n2 -> n1 = n2
  | BoolVal b1, BoolVal b2 -> b1 = b2
  | ListVal l1, ListVal l2 -> is_same_list l1 l2
  | _, _ -> failwith "unexpected expr"

and greater v1 v2 =
  match (v1, v2) with
  | IntVal n1, IntVal n2 -> n1 > n2
  | _, _ -> failwith "unexpected expr"

and less v1 v2 =
  match (v1, v2) with
  | IntVal n1, IntVal n2 -> n1 < n2
  | _, _ -> failwith "unexpected expr"

(* 関数適用 *)
and app funpart arg cont =
  match funpart with
  | FunVal (x, body, env1) -> eval body (ext env1 x arg) cont
  | RecFunVal (f, x, body, env1) ->
      let env2 = ext (ext env1 x arg) f funpart in
      eval body env2 cont
  | _ -> failwith "unexpected expr"