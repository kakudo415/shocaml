type token =
  | Integer of string
  | AddOp
  | SubOp
  | MulOp
  | QuoOp
  | RemOp
  | LParen
  | RParen

let rec num_lexer source =
  match source with
    | [] -> "", source
    | head :: future ->
      match head with
        | '0' .. '9' ->
          let digits, future = num_lexer future in
          (String.make 1 head) ^ digits, future (* TODO: (String.make 1 head)をいい感じにする *)
        | _ -> "", source

(* [char] -> [token] *)
let rec lexer source =
  match source with
    | [] -> []
    | head :: future ->
      match head with
        | '0' .. '9' ->
          let num, future = num_lexer source in
          Integer num :: lexer future
        | '+' -> AddOp :: lexer future
        | '-' -> SubOp :: lexer future
        | '*' -> MulOp :: lexer future
        | '/' -> QuoOp :: lexer future
        | '%' -> RemOp :: lexer future
        | '(' -> LParen :: lexer future
        | ')' -> RParen :: lexer future
        | ' ' | '\t' | '\n' -> lexer future
        | _ -> []

let source_string = "123 * (456 + 789)"
let _ = lexer (List.init (String.length source_string) (String.get source_string))
