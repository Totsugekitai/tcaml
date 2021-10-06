open Syntax
open Env

(* 型の定義 *)
type tyvar = string

type ty = TInt | TBool | TVar of tyvar | TArrow of ty * ty

(* 型環境 *)
type tyenv = (string * ty) list

(* 型代入 *)
type tysubst = (tyvar * ty) list

(* 型検査 *)
(* check : tyenv -> exp -> ty *)
let rec check te e =
  match e with
  | Var s -> lookup s te
  | IntLit _ -> TInt
  | BoolLit _ -> TBool
  | If (e1, e2, e3) -> (
      let t1 = check te e1 in
      let t2 = check te e2 in
      let t3 = check te e3 in
      match t1 with
      | TBool when t2 = t3 -> t2
      | _ -> failwith "type error in If")
  | Let (x, e1, e2) ->
      let tx = lookup x te in
      let t1 = check te e1 in
      let t2 = check te e2 in
      if tx = t1 then t2 else failwith "type error in Let"
  (* | LetRec (f, x, e1, e2) ->
      let tf = Eval.lookup f te in
      let tx = Eval.lookup x te in
      let t1 = check te e1 in
      let t2 = check te e2 in *)
  | Fun (x, e1) ->
      let t1 = lookup x te in
      let t2 = check te e1 in
      TArrow (t1, t2)
  | App (e1, e2) -> (
      let t1 = check te e1 in
      let t2 = check te e2 in
      match t1 with
      | TArrow (t10, t11) ->
          if t2 = t10 then t11 else failwith "type error in App"
      | _ -> failwith "type error in App")
  | Eq (e1, e2) -> (
      match (check te e1, check te e2) with
      | TInt, TInt -> TBool
      | TBool, TBool -> TBool
      | _ -> failwith "type error in Eq")
  | InEq (e1, e2) -> (
      match (check te e1, check te e2) with
      | TInt, TInt -> TBool
      | TBool, TBool -> TBool
      | _ -> failwith "type error in InEq")
  | Greater (e1, e2) -> (
      match (check te e1, check te e2) with
      | TInt, TInt -> TBool
      | _ -> failwith "type error in Greater")
  | Less (e1, e2) -> (
      match (check te e1, check te e2) with
      | TInt, TInt -> TBool
      | _ -> failwith "type error in Less")
  | Plus (e1, e2) -> (
      match (check te e1, check te e2) with
      | TInt, TInt -> TInt
      | _ -> failwith "type error int Plus")
  | Minus (e1, e2) -> (
      match (check te e1, check te e2) with
      | TInt, TInt -> TInt
      | _ -> failwith "type error int Minus")
  | Times (e1, e2) -> (
      match (check te e1, check te e2) with
      | TInt, TInt -> TInt
      | _ -> failwith "type error int Times")
  | Div (e1, e2) -> (
      match (check te e1, check te e2) with
      | TInt, TInt -> TInt
      | _ -> failwith "type error int Div")
  | _ -> failwith "unknown expression"

let theta0 = ([] : tysubst)

(* new_typevar : int -> ty * int *)
let new_typevar n = (TVar ("'a" ^ string_of_int n), n + 1)

(* substitute t1 TInt te1 で「型環境te1における型変数t1をTIntに変更した型環境」を返す *)
(* substitute : ty -> ty -> tyenv -> tyenv *)
let rec substitute tvar t te =
  match te with
  | [] -> []
  | (x, t2) :: te2 ->
      let t3 = if t2 = tvar then t else t2 in
      (x, t3) :: substitute tvar t te2

(* tがtxの型変数を含まないかどうかのチェック *)
let rec occurs tx t =
  if tx = t then true
  else
    match t with TArrow (t1, t2) -> occurs tx t1 || occurs tx t2 | _ -> false

(* 代入thetaを型tに適用する *)
let rec subst_ty theta t =
  let rec subst_ty1 theta1 s =
    match theta1 with
    | [] -> TVar s
    | (tx, t1) :: theta2 -> if tx = s then t1 else subst_ty1 theta2 s
  in
  match t with
  | TInt -> TInt
  | TBool -> TBool
  | TArrow (t2, t3) -> TArrow (subst_ty theta t2, subst_ty theta t3)
  | TVar s -> subst_ty1 theta s

(* 代入thetaを型環境teに適用する *)
let subst_tyenv theta te = List.map (fun (x, t) -> (x, subst_ty theta t)) te

(* 代入thetaを型の等式リストeqlに適用する *)
let subst_eql theta eql =
  List.map (fun (t1, t2) -> (subst_ty theta t1, subst_ty theta t2)) eql

(* 2つの代入を合成して返す。theta1が先でtheta2が後 *)
let compose_subst theta2 theta1 =
  let theta11 = List.map (fun (tx, t) -> (tx, subst_ty theta2 t)) theta1 in
  List.fold_left
    (fun tau (tx, t) ->
      try
        let _ = lookup tx theta1 in
        tau
      with Failure _ -> (tx, t) :: tau)
    theta11 theta2

(* 単一化 *)
(* unify : (ty * ty) list -> tysubst *)
let unify eql =
  (* eql : 型同士の等式のリスト
   * theta : 作りかけの型代入
   *)
  let rec solve eql theta =
    match eql with
    | [] -> theta
    | (t1, t2) :: eql2 -> (
        if t1 = t2 then solve eql2 theta
        else
          match (t1, t2) with
          | TArrow (t11, t12), TArrow (t21, t22) ->
              solve ((t11, t21) :: (t12, t22) :: eql2) theta
          | TVar s, _ ->
              if occurs t1 t2 then failwith "unification failed"
              else
                solve
                  (subst_eql [ (s, t2) ] eql2)
                  (compose_subst [ (s, t2) ] theta)
          | _, TVar s ->
              if occurs t2 t1 then failwith "unification failed"
              else
                solve
                  (subst_eql [ (s, t1) ] eql2)
                  (compose_subst [ (s, t1) ] theta)
          | _, _ -> failwith "unification failed")
  in
  solve eql []

(* 型推論 *)
(* inf : tyenv -> exp -> int -> tyenv * ty * tysubst * int *)
let rec inf te e n =
  match e with
  | Var s -> (
      try
        let t1 = lookup s te in
        (te, t1, theta0, n)
      with Failure _ ->
        let tx, n1 = new_typevar n in
        let te1 = ext te s tx in
        (te1, tx, theta0, n1))
  | IntLit _ -> (te, TInt, theta0, n)
  | BoolLit _ -> (te, TBool, theta0, n)
  | Plus (e1, e2) ->
      let te1, t1, theta1, n1 = inf te e1 n in
      let te2, t2, theta2, n2 = inf te1 e2 n1 in
      let t11 = subst_ty theta2 t1 in
      let theta3 = unify [ (t11, TInt); (t2, TInt) ] in
      let te3 = subst_tyenv theta3 te2 in
      let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
      (te3, TInt, theta4, n2)
  | _ -> failwith "type inference error: unknown expression"
