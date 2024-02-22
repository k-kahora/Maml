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
  | EQ
  | NOT_EQ
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

let token_to_string_debug (t : token_name) : string =
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
      "ASSIGN"
  | PLUS ->
      "PLUS"
  | MINUS ->
      "MINUS"
  | BANG ->
      "BANG"
  | ASTERISK ->
      "ASTERISK"
  | SLASH ->
      "SLASH"
  | LT ->
      "LT"
  | GT ->
      "GT"
  | EQ ->
      "EQ"
  | NOT_EQ ->
      "NOT_EQ"
  (* Delimeters *)
  | COMMA ->
      "COMMA"
  | SEMICOLON ->
      "SEMICOLON"
  | LPAREN ->
      "LPAREN"
  | RPAREN ->
      "RPAREN"
  | LBRACE ->
      "LBRACE"
  | RBRACE ->
      "RBRACE"
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
  | EQ ->
      "=="
  | NOT_EQ ->
      "!="
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
