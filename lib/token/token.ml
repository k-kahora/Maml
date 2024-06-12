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
  | LTEQ
  | GT
  | GTEQ
  | EQ
  | NOT_EQ
  (* Delimeters *)
  | COMMA
  | SEMICOLON
  | COLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  (* Keywords *)
  | FUNCTION
  | LET
  | TRUE
  | FALSE
  | IF
  | ELSE
  | RETURN
  | STRING

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
  | LTEQ ->
      "LTEQ"
  | GT ->
      "GT"
  | GTEQ ->
      "GTEQ"
  | EQ ->
      "EQ"
  | NOT_EQ ->
      "NOT_EQ"
  (* Delimeters *)
  | COMMA ->
      "COMMA"
  | COLON ->
      "COLON"
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
  | LBRACKET ->
      "LBRACKET"
  | RBRACKET ->
      "RBRACKET"
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
  | STRING ->
      "STRING"

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
  | LTEQ ->
      "<="
  | GTEQ ->
      ">="
  | NOT_EQ ->
      "!="
  (* Delimeters *)
  | COMMA ->
      ","
  | COLON ->
      ":"
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
  | RBRACKET ->
      "["
  | LBRACKET ->
      "]"
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
  | STRING ->
      "STRING"

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

let dummy_token = {type'= FALSE; literal= "false"}
