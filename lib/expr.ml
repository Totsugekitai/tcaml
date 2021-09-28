type expr =
  | IntLit of int   (* 整数リテラル *)
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | BoolLit of bool (* 真理値リテラル *)
  | If of expr * expr * expr
  | Eq of expr * expr
  | Greater of expr * expr
  | Less of expr * expr

let rec eval e =
  let binop f e1 e2 =
    match (eval e1, eval e2) with
    | (Value.IntVal(n1), Value.IntVal(n2)) -> Value.IntVal(f n1 n2)
    | _ -> failwith "integer value expected"
  in
  match e with
  | IntLit(n) -> Value.IntVal(n)
  | Add(e1, e2) -> binop (+) e1 e2
  | Sub(e1, e2) -> binop (-) e1 e2
  | Mul(e1, e2) -> binop ( * ) e1 e2
  | Div(e1, e2) -> if (eval e2) = Value.IntVal(0) then failwith "zero division" else binop (/) e1 e2
  | BoolLit(b) -> Value.BoolVal(b)
  | If(e1, e2, e3) ->
    begin
      match (eval e1) with
      | Value.BoolVal(true) -> eval e2
      | Value.BoolVal(false) -> eval e3
      | _ -> failwith "wrong value"
    end
  | Eq(e1, e2) ->
    begin
      match (eval e1, eval e2) with
      | (Value.IntVal(n1), Value.IntVal(n2)) -> Value.BoolVal(n1 = n2)
      | (Value.BoolVal(b1), Value.BoolVal(b2)) -> Value.BoolVal(b1 = b2)
      | _ -> failwith "wrong value"
    end
  | Greater(e1, e2) ->
    begin
      match (eval e1, eval e2) with
      | (Value.IntVal(n1), Value.IntVal(n2)) -> Value.BoolVal(n1 > n2)
      | _ -> failwith "wrong value"
    end
  | Less(e1, e2) ->
    begin
      match (eval e1, eval e2) with
      | (Value.IntVal(n1), Value.IntVal(n2)) -> Value.BoolVal(n1 < n2)
      | _ -> failwith "wrong value"
    end