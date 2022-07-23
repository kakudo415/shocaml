(* T Y P E S *)
(* ハンガリアン記法なのは、モジュール分けせずに作成するため。苦肉の策。 *)

type token =
  | T_Identifier of string
  | T_Integer of string
  | T_Eq
  | T_Add
  | T_Sub
  | T_Mul
  | T_Quo
  | T_Rem
  | T_LParen
  | T_RParen
  | T_Let
  | T_And
  | T_In

(* TODO: 各ノードが取りうる型のみ子要素になるように定義を絞る *)
type node =
  | N_Integer of string
  | N_Identifier of string
  | N_LetIn of node * node
  | N_Bindings of (string * node) list
  | N_BinOp of token * node * node
  | N_Error
  | N_EOF


(* L E X E R *)

let rec int_lexer source =
  match source with
    | [] -> ("", source)
    | head :: future ->
      match head with
        | '0' .. '9' ->
          let digits, future = int_lexer future in
          ((String.make 1 head) ^ digits, future) (* TODO: (String.make 1 head)をいい感じにする。できないかも？ | '0' -> "0" ^ digits にすればできるが･･･ *)
        | _ -> ("", source)

and ident_lexer source =
  match source with
    | [] -> ("", source)
    | head :: future ->
      match head with
        | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' | '\'' ->
          let ident, future = ident_lexer future in
          ((String.make 1 head) ^ ident, future) (* TODO: (String.make 1 head)をいい感じにする。できないかも？ *)
        | _ -> ("", source)

let rec lexer source =
  match source with
    | [] -> []
    | head :: future ->
      match head with
        | 'a' .. 'z' | 'A' .. 'Z' | '_' -> (
          let ident, future = ident_lexer source in
          match ident with
            | "let" -> T_Let :: lexer future
            | "and" -> T_And :: lexer future
            | "in" -> T_In :: lexer future
            | _ -> T_Identifier ident :: lexer future
        )
        | '0' .. '9' ->
          let num, future = int_lexer source in
          T_Integer num :: lexer future
        | '=' -> T_Eq :: lexer future
        | '+' -> T_Add :: lexer future
        | '-' -> T_Sub :: lexer future
        | '*' -> T_Mul :: lexer future
        | '/' -> T_Quo :: lexer future
        | '%' -> T_Rem :: lexer future
        | '(' -> T_LParen :: lexer future
        | ')' -> T_RParen :: lexer future
        | ' ' | '\t' | '\n' -> lexer future
        | _ -> []


(* P A R S E R *)

let rec expr_parser tokens =
  match tokens with
    | T_Let :: future -> (
      let bindings, future = binding_parser future in
      match future with
        | T_In :: future ->
          let expr, future = expr_parser future in
          (N_LetIn (N_Bindings bindings, expr), future)
        | _ -> (N_Error, future)
    )
    | _ -> add_parser tokens

and binding_parser tokens =
  match tokens with
    | T_Identifier name :: T_Eq :: future -> (
      let expr, future = expr_parser future in
      match future with
        | T_And :: future ->
          let bindings, future = binding_parser future in
          ((name, expr) :: bindings, future)
        | _ -> ([(name, expr)], future)
    )
    | _ -> ([], tokens)

(* <add> ::= <mul> <add'> *)
and add_parser tokens =
  let lhs, future = mul_parser tokens in
  add'_parser lhs future

(* <add'> ::= "+" <mul> <add'> | "" *)
and add'_parser lhs tokens =
  match tokens with
    | T_Add :: future ->
      let rhs, future = mul_parser future in
      let root = N_BinOp (T_Add, lhs, rhs) in
      add'_parser root future
    | T_Sub :: future ->
      let rhs, future = mul_parser future in
      let root = N_BinOp (T_Sub, lhs, rhs) in
      add'_parser root future
    | _ -> (lhs, tokens)

(* <mul> ::= <elm> <mul'> *)
and mul_parser tokens =
  let lhs, future = elm_parser tokens in
  mul'_parser lhs future

(* <mul'> ::= "*" <elm> <mul'> | "" *)
and mul'_parser lhs tokens =
  match tokens with
    | T_Mul :: future ->
      let rhs, future = elm_parser future in
      let root = N_BinOp (T_Mul, lhs, rhs) in
      mul'_parser root future
    | T_Quo :: future ->
      let rhs, future = elm_parser future in
      let root = N_BinOp (T_Quo, lhs, rhs) in
      mul'_parser root future
    | T_Rem :: future ->
      let rhs, future = elm_parser future in
      let root = N_BinOp (T_Rem, lhs, rhs) in
      mul'_parser root future
    | _ -> (lhs, tokens)

(* <elm> ::= "(" <add> ")" | <int> *)
and elm_parser tokens =
  match tokens with
    | T_Integer num :: future -> (N_Integer num, future)
    | T_Identifier name :: future -> (N_Identifier name, future)
    | T_LParen :: future -> (
      let root, future = add_parser future in
      match future with
        | T_RParen :: future -> (root, future)
        | _ -> (N_Error, future)
    )
    | _ -> (N_Error, tokens)

let rec parser tokens =
  match tokens with
    | [] -> N_EOF
    | head :: future ->
      match head with
        (* 現状、式以外の構文は無いです *)
        | _ ->
          let expr, future = expr_parser tokens in
          expr


(* A N A L Y Z E R *)

let rec analyzer ast =
  match ast with
    | N_LetIn (bindings, _) ->
      bindings_analyzer bindings
    | _ -> []

and bindings_analyzer bindings =
  match bindings with
    | [] -> []
    | (name, expr) :: bindings ->
      name :: bindings_analyzer bindings


(* E M I T T E R *)

let rec emitter ast =
  match ast with
    | N_EOF -> ""
    | N_Integer num ->
      "  pushq $" ^ num ^ "\n"
    | N_Identifier name ->
      
    | N_BinOp (op, lhs, rhs) -> (
      (emitter lhs) ^
      (emitter rhs) ^
      "  popq %rdi\n" ^
      "  popq %rax\n" ^
      match op with
        | T_Add -> "  addq %rdi, %rax\n  pushq %rax\n"
        | T_Sub -> "  subq %rdi, %rax\n  pushq %rax\n"
        | T_Mul -> "  imulq %rdi, %rax\n  pushq %rax\n"
        | T_Quo -> "  cqto\n  idivq %rdi\n  pushq %rax\n"
        | T_Rem -> "  cqto\n  idivq %rdi\n  pushq %rdx\n"
        | _ -> "  ERROR\n"
    )
    | _ -> "  ERROR\n"


(* MAIN *)

let rec string_of_ast level ast =
  (indent_from_level level) ^ match ast with
    | N_Integer num -> num ^ "\n"
    | N_Identifier name -> name ^ "\n"
    | N_BinOp (op, lhs, rhs) -> (
      (
        match op with
          | T_Add -> "Add\n"
          | T_Sub -> "Sub\n"
          | T_Mul -> "Mul\n"
          | T_Quo -> "Quo\n"
          | T_Rem -> "Rem\n"
          | _ -> "ERROR\n"
      )
      ^ (string_of_ast (level + 1) lhs)
      ^ (string_of_ast (level + 1) rhs)
    )
    | N_LetIn (bindings, expr) ->
      (string_of_ast level bindings) ^
      (indent_from_level level) ^ "In\n" ^
      (string_of_ast (level + 1) expr)
    | N_Bindings bindings -> (
      match bindings with
        | [] -> ""
        | (name, expr) :: bindings ->
          "Binding " ^ name ^ " = \n"
          ^ (string_of_ast (level + 1) expr)
          ^ (string_of_ast level (N_Bindings bindings))
    )
    | _ -> "ERROR\n"

and indent_from_level level =
  match level with
    | 0 -> ""
    | level -> "  " ^ indent_from_level (level - 1)

let source_string = "let hoge = 123 in hoge"
let source_chars = List.init (String.length source_string) (String.get source_string)
let tokens = lexer source_chars
let ast = parser tokens
let _ = prerr_string (string_of_ast 0 ast)
let symbols = analyzer ast
let code = emitter ast symbols
let _ = print_string (
  ".text\n" ^
  ".globl main\n" ^
  "main:\n" ^
  code ^
  "  pop %rax\n" ^
  "  ret\n"
)
