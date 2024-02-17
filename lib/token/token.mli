type token_name =
  | ILLEGAL
  | EOF
  | IDENT
  | INT
  | ASSIGN
  | PLUS
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | FUNCTION
  | LET

val token_to_string : token_name -> string

type token = {type': token_name; literal: string}

val look_up_ident : string -> token_name
