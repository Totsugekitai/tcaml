type token =
  (* リテラル *)
  | IntLit of int
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  (* 識別子 *)
  | CapitalIdent of string
  | LowerIdent of string
  (* 中置演算子 *)
  | Infix of string
  (* 算術演算子 *)
  | Plus
  | Minus
  | Asterisk
  | Slash
  (* 比較演算子 *)
  | Equal
  | Greater
  | Less
  (* 制御記号 *)
  | LParen
  | RParen
  | Dot
  | Comma
  | Arrow
  | Vertical
  (* キーワード *)
  | Let
  | Rec
  | Type
  | Of
  | Match
  | With
  | In
  | If
  | Then
  | Else
  | DoubleQuote
  (* それ以外 *)
  | Unknown

let empty_token = []

let skip_list = [ ' '; '\t'; '\n'; '\r' ]

let is_digit = function '0' .. '9' -> true | _ -> false

let to_digit c =
  match c with
  | '0' .. '9' -> int_of_char c - int_of_char '0'
  | _ -> failwith ("unexpected char: " ^ Char.escaped c ^ " is not digit")

let is_lower = function 'a' .. 'z' | '_' | '\'' -> true | _ -> false

let is_capital = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '\'' -> true
  | _ -> false

let lexer src =
  let rec aux acc i =
    (* 文字列から1文字を読む *)
    let read_char i =
      try (i + 1, Some src.[i]) with Invalid_argument _ -> (i, None)
    in
    (* 連続した数字の文字列をIntLitに変換 *)
    let rec read_int acc i =
      let maybe_c = read_char i in
      match maybe_c with
      | i, Some c when is_digit c -> read_int ((acc * 10) + to_digit c) i
      | i, Some _ -> (acc, i - 1)
      | _, None -> (acc, i)
    in
    (* 連続したlower caseの識別子文字列をLowerIdentに変換 *)
    let rec read_lower_ident acc i =
      let maybe_c = read_char i in
      match maybe_c with
      | i, Some c when is_lower c -> read_lower_ident (acc ^ Char.escaped c) i
      | i, Some _ -> (acc, i - 1)
      | _, None -> (acc, i)
    in
    let rec read_capital_ident acc i =
      let maybe_c = read_char i in
      match maybe_c with
      | i, Some c when is_capital c ->
          read_capital_ident (acc ^ Char.escaped c) i
      | i, Some _ -> (acc, i - 1)
      | _, None -> (acc, i)
    in
    (* 文字を振り分ける処理 *)
    match read_char i with
    | _, None -> acc
    | i, Some c -> (
        match c with
        (* 読み飛ばすやつ *)
        | c when List.exists (fun x -> x = c) skip_list -> aux acc i
        (* 数字 *)
        | c when is_digit c ->
            let num, i = read_int 0 (i - 1) in
            aux (acc @ [ IntLit num ]) i
        (* 識別子 *)
        | c when is_lower c ->
            let ident, i = read_lower_ident "" (i - 1) in
            aux (acc @ [ LowerIdent ident ]) i
        | c when is_capital c ->
            let ident, i = read_capital_ident "" (i - 1) in
            aux (acc @ [ CapitalIdent ident ]) i
        (* 演算子 *)
        | '+' -> aux (acc @ [ Plus ]) i
        | '-' -> aux (acc @ [ Minus ]) i
        | '*' -> aux (acc @ [ Asterisk ]) i
        | '/' -> aux (acc @ [ Slash ]) i
        (* その他 *)
        | _ -> failwith "unknown character")
  in
  aux [] 0
