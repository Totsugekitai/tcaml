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
  (* | Infix of string *)
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
  | Quote
  | DoubleQuote
  | At
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
  | When
  | Fun
  | Function
  (* それ以外 *)
  | Unknown

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
      | i, Some c when is_lower c ->
          (* クオートだけ特殊文字でエスケープして返されるのでこうなってる *)
          if c != '\'' then read_lower_ident (acc ^ Char.escaped c) i
          else read_lower_ident (acc ^ "'") i
      | i, Some _ -> (acc, i - 1)
      | _, None -> (acc, i)
    in
    (* 連続したcapital caseの識別子文字列をCapitalIdentに変換 *)
    let rec read_capital_ident acc i =
      let maybe_c = read_char i in
      match maybe_c with
      | i, Some c when is_capital c ->
          (* クオートだけ特殊文字でエスケープして返されるのでこうなってる *)
          if c != '\'' then read_capital_ident (acc ^ Char.escaped c) i
          else read_capital_ident (acc ^ "'") i
      | i, Some _ -> (acc, i - 1)
      | _, None -> (acc, i)
    in
    (* コメントを読み飛ばす処理 *)
    let rec read_comment acc i nest_level =
      let maybe_c = read_char i in
      match maybe_c with
      | i, Some c when c = '(' -> (
          let maybe_c = read_char i in
          match maybe_c with
          | i, Some c when c = '*' -> read_comment acc i (nest_level + 1)
          | i, Some _ -> (acc @ [ LParen ], i - 1)
          | i, None -> (acc, i))
      | i, Some c when c = '*' -> (
          let maybe_c = read_char i in
          match maybe_c with
          | i, Some c when c = ')' ->
              if nest_level = 0 then (acc, i)
              else read_comment acc i (nest_level - 1)
          | i, Some _ -> read_comment acc i nest_level
          | _, None -> failwith "comment is not closed")
      | i, Some _ -> read_comment acc i nest_level
      | i, None -> (acc, i)
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
        (* 記号 *)
        | '+' -> aux (acc @ [ Plus ]) i
        | '-' -> aux (acc @ [ Minus ]) i
        | '*' -> aux (acc @ [ Asterisk ]) i
        | '/' -> aux (acc @ [ Slash ]) i
        | '=' -> aux (acc @ [ Equal ]) i
        | '>' -> aux (acc @ [ Greater ]) i
        | '<' -> aux (acc @ [ Less ]) i
        (* | '(' -> aux (acc @ [ LParen ]) i *)
        | '(' -> (
            let maybe_c = read_char i in
            match maybe_c with
            | i, Some c when c = '*' ->
                let acc, i = read_comment acc i 0 in
                aux acc i
            | i, Some _ -> aux (acc @ [ LParen ]) (i - 1)
            | _, None -> acc)
        | ')' -> aux (acc @ [ RParen ]) i
        | '.' -> aux (acc @ [ Dot ]) i
        | ',' -> aux (acc @ [ Comma ]) i
        | '|' -> aux (acc @ [ Vertical ]) i
        | '\'' -> aux (acc @ [ Quote ]) i
        | '"' -> aux (acc @ [ DoubleQuote ]) i
        | '@' -> aux (acc @ [ At ]) i
        (* 識別子と予約語 *)
        | c when is_lower c -> (
            let ident, i = read_lower_ident "" (i - 1) in
            match ident with
            | "let" -> aux (acc @ [ Let ]) i
            | "rec" -> aux (acc @ [ Rec ]) i
            | "type" -> aux (acc @ [ Type ]) i
            | "of" -> aux (acc @ [ Of ]) i
            | "match" -> aux (acc @ [ Match ]) i
            | "with" -> aux (acc @ [ With ]) i
            | "in" -> aux (acc @ [ In ]) i
            | "if" -> aux (acc @ [ If ]) i
            | "then" -> aux (acc @ [ Then ]) i
            | "else" -> aux (acc @ [ Else ]) i
            | "when" -> aux (acc @ [ When ]) i
            | "fun" -> aux (acc @ [ Fun ]) i
            | "function" -> aux (acc @ [ Function ]) i
            | _ -> aux (acc @ [ LowerIdent ident ]) i)
        | c when is_capital c ->
            let ident, i = read_capital_ident "" (i - 1) in
            aux (acc @ [ CapitalIdent ident ]) i
        (* その他 *)
        | _ -> failwith "unknown character")
  in
  aux [] 0
