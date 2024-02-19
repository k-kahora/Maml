(* ttype':ype token_type = string *)
type token_name =
  | ILLEGAL
  | EOF
  (* Identifies and literals *)
  | IDENT
  | INT
  (* Operators *)
  | ASSIGN
  | PLUS
  | MINUS
  | BANG
  | ASTERISK
  | SLASH
  | LT
  | GT
  (* Delimeters *)
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  (* Keywords *)
  | FUNCTION
  | LET
  | TRUE
  | FALSE
  | IF
  | ELSE
  | RETURN

let token_to_string (t : token_name) : string =
  match t with
  | ILLEGAL ->
      "ILLEGAL"
  | EOF ->
      "EOF"
  (* Identifies and literals *)
  | IDENT ->
      "IDENT"
  | INT ->
      "INT"
  (* Operators *)
  | ASSIGN ->
      "="
  | PLUS ->
      "+"
  | MINUS ->
      "-"
  | BANG ->
      "!"
  | ASTERISK ->
      "*"
  | SLASH ->
      "/"
  | LT ->
      "<"
  | GT ->
      ">"
  (* Delimeters *)
  | COMMA ->
      ","
  | SEMICOLON ->
      ";"
  | LPAREN ->
      "("
  | RPAREN ->
      ")"
  | LBRACE ->
      "{"
  | RBRACE ->
      "}"
  (* Keywords *)
  | FUNCTION ->
      "FUNCTION"
  | LET ->
      "LET"
  | TRUE ->
      "TRUE"
  | FALSE ->
      "FALSE"
  | IF ->
      "IF"
  | ELSE ->
      "ELSE"
  | RETURN ->
      "RETURN"

type token = {type': token_name; literal: string}

let look_up_ident (s : string) : token_name =
  match s with
  | "fn" ->
      FUNCTION
  | "let" ->
      LET
  | "if" ->
      IF
  | "else" ->
      ELSE
  | "return" ->
      RETURN
  | "true" ->
      TRUE
  | "false" ->
      FALSE
  | _ ->
      IDENT
