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

val token_to_string : token_name -> string

val token_to_string_debug : token_name -> string

type token = {type': token_name; literal: string}

val look_up_ident : string -> token_name
