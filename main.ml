(* ハンガリアン記法なのは、モジュール分けせずに作成するため。苦肉の策。 *)
type token =
  | T_Integer of string
  | T_Add
  | T_Sub
  | T_Mul
  | T_Quo
  | T_Rem
  | T_LParen
  | T_RParen

type node =
  | N_Integer of string
  | N_BinOp of token * node * node (* TODO: 演算子が取りうる子要素に定義を絞る *)
  | N_EOF

(* [char] -> string, [char] *)
let rec int_lexer source =
  match source with
    | [] -> "", source
    | head :: future ->
      match head with
        | '0' .. '9' ->
          let digits, future = int_lexer future in
          (String.make 1 head) ^ digits, future (* TODO: (String.make 1 head)をいい感じにする。できないかも？ *)
        | _ -> "", source

(* [char] -> [token] *)
let rec lexer source =
  match source with
    | [] -> []
    | head :: future ->
      match head with
        | '0' .. '9' ->
          let num, future = int_lexer source in
          T_Integer num :: lexer future
        | '+' -> T_Add :: lexer future
        | '-' -> T_Sub :: lexer future
        | '*' -> T_Mul :: lexer future
        | '/' -> T_Quo :: lexer future
        | '%' -> T_Rem :: lexer future
        | '(' -> T_LParen :: lexer future
        | ')' -> T_RParen :: lexer future
        | ' ' | '\t' | '\n' -> lexer future
        | _ -> []

(* FIXME: 左結合にしなくちゃ！！！！ *)
(* [tokens] -> node *)
let rec addsub_parser tokens =
  let lhs, future = mulquorem_parser tokens in
  match future with
    | T_Add :: future ->
      let rhs, future = addsub_parser future in
      N_BinOp (T_Add, lhs, rhs), future
    | T_Sub :: future ->
      let rhs, future = addsub_parser future in
      N_BinOp (T_Sub, lhs, rhs), future
    | _ -> lhs, future

(* [tokens] -> node *)
and mulquorem_parser tokens =
  let lhs, future = elm_parser tokens in
  match future with
    | T_Mul :: future ->
      let rhs, future = mulquorem_parser future in
      N_BinOp (T_Mul, lhs, rhs), future
    | T_Quo :: future ->
      let rhs, future = mulquorem_parser future in
      N_BinOp (T_Quo, lhs, rhs), future
    | T_Rem :: future ->
      let rhs, future = mulquorem_parser future in
      N_BinOp (T_Rem, lhs, rhs), future
    | _ -> lhs, future

(* [tokens] -> node *)
and elm_parser tokens =
  match tokens with
    | T_Integer num :: future -> N_Integer num, future
    | T_LParen :: future ->
      let expr, future = addsub_parser future in
      match future with
        | T_RParen :: _ -> expr, future
        | _ -> (* ERROR *) (N_EOF, future)

(* [token] -> node *)
let rec parser tokens =
  match tokens with
    | [] -> N_EOF
    | head :: future ->
      match head with
        | T_Integer _ | T_LParen ->
          let expr, future = addsub_parser tokens in
            expr

let source_string = "(1 + 2 - 3) / 4 + 5 * 6"
let source_chars = List.init (String.length source_string) (String.get source_string)
let _ = parser (lexer source_chars)
