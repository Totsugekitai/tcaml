open Syntax

(* 型の定義 *)
type ty = TInt | TBool

(* 型検査 *)
(* check : exp -> ty *)
let rec check e =
  match e with
  | IntLit _ -> TInt
  | BoolLit _ -> TBool
  | Plus (e1, e2) -> (
      match (check e1, check e2) with
      | TInt, TInt -> TInt
      | _ -> failwith "type error int Plus")
  | If (e1, e2, e3) -> (
      match (check e1, check e2, check e3) with
      | TBool, TInt, TInt -> TInt
      | TBool, TBool, TBool -> TBool
      | _ -> failwith "type error in If")
  | _ -> failwith "unknown expression"
