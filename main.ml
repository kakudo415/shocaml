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
  | N_Error
  | N_EOF

(* [char] -> (string, [char]) *)
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

(* <add> ::= <mul> <add'> *)
let rec add_parser tokens =
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
    | T_LParen :: future -> (
        let root, future = add_parser future in
        match future with
          | T_RParen :: future -> (root, future)
          | _ -> (N_Error, future)
      )
    | _ -> (N_Error, tokens)

(* [token] -> node *)
let rec parser tokens =
  match tokens with
    | [] -> N_EOF
    | head :: future ->
      match head with
        | T_Integer _ | T_LParen ->
          let expr, future = add_parser tokens in
            expr
        | _ -> N_Error

let rec emitter ast =
  match ast with
    | N_EOF -> ""
    | N_Integer num ->
      "  pushq $" ^ num ^ "\n"
    | N_BinOp (op, lhs, rhs) -> (
        match op with
          | T_Add ->
            let lasm = emitter lhs in
            let rasm = emitter rhs in
            lasm ^ rasm ^ "  popq %rdi\n  popq %rax\n  addq %rdi, %rax\n  pushq %rax\n"
          | T_Sub ->
            let lasm = emitter lhs in
            let rasm = emitter rhs in
            lasm ^ rasm ^ "  popq %rdi\n  popq %rax\n  subq %rdi, %rax\n  pushq %rax\n"
          | T_Mul ->
            let lasm = emitter lhs in
            let rasm = emitter rhs in
            lasm ^ rasm ^ "  popq %rdi\n  popq %rax\n  imulq %rdi, %rax\n  pushq %rax\n"
          | T_Quo ->
            let lasm = emitter lhs in
            let rasm = emitter rhs in
            lasm ^ rasm ^ "  popq %rdi\n  popq %rax\n  cqto\n  idivq %rdi\n  pushq %rax\n"
          | T_Rem ->
            let lasm = emitter lhs in
            let rasm = emitter rhs in
            lasm ^ rasm ^ "  popq %rdi\n  popq %rax\n  cqto\n  idivq %rdi\n  pushq %rdx\n"
          | _ -> "  ERROR\n"
      )
    | _ -> "  ERROR\n"

let source_string = "1 + (2 + 3) + 4 + 5 * 6 / 7 - 8 * 9 % 10"
let source_chars = List.init (String.length source_string) (String.get source_string)
let tokens = lexer source_chars
let ast = parser tokens
let code = emitter ast
let _ = print_string (".text\n.globl main\nmain:\n" ^ code ^ "  pop %rax\n  ret\n")
