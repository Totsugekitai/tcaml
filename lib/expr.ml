type expr =
  (* リテラル *)
  | IntLit of int
  | BoolLit of bool
  (* 算術演算 *)
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  (* 比較 *)
  | Eq of expr * expr
  | Greater of expr * expr
  | Less of expr * expr
  (* 制御構文 *)
  | If of expr * expr * expr
  (* 環境 *)
  | Var of string
  | Let of string * expr * expr

let rec eval e env =
  let binop f e1 e2 env =
    match (eval e1 env, eval e2 env) with
    | Value.IntVal n1, Value.IntVal n2 -> Value.IntVal (f n1 n2)
    | _ -> failwith "integer value expected"
  in
  match e with
  (* リテラル *)
  | IntLit n -> Value.IntVal n
  | BoolLit b -> Value.BoolVal b
  (* 算術演算 *)
  | Add (e1, e2) -> binop ( + ) e1 e2 env
  | Sub (e1, e2) -> binop ( - ) e1 e2 env
  | Mul (e1, e2) -> binop ( * ) e1 e2 env
  | Div (e1, e2) ->
      if eval e2 env = Value.IntVal 0 then failwith "zero division"
      else binop ( / ) e1 e2 env
  (* 比較 *)
  | Eq (e1, e2) -> (
      match (eval e1 env, eval e2 env) with
      | Value.IntVal n1, Value.IntVal n2 -> Value.BoolVal (n1 = n2)
      | Value.BoolVal b1, Value.BoolVal b2 -> Value.BoolVal (b1 = b2)
      | _ -> failwith "wrong value")
  | Greater (e1, e2) -> (
      match (eval e1 env, eval e2 env) with
      | Value.IntVal n1, Value.IntVal n2 -> Value.BoolVal (n1 > n2)
      | _ -> failwith "wrong value")
  | Less (e1, e2) -> (
      match (eval e1 env, eval e2 env) with
      | Value.IntVal n1, Value.IntVal n2 -> Value.BoolVal (n1 < n2)
      | _ -> failwith "wrong value")
  (* 制御構文 *)
  | If (e1, e2, e3) -> (
      match eval e1 env with
      | Value.BoolVal true -> eval e2 env
      | Value.BoolVal false -> eval e3 env
      | _ -> failwith "wrong value")
  (* 環境 *)
  | Var x -> Env.lookup x env
  | Let (x, e1, e2) ->
      let env1 = Env.ext env x (eval e1 env) in
      eval e2 env1
