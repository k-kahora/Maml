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

type token = {type': token_name; literal: string}

let look_up_ident (s : string) : token_name =
  match s with "fn" -> FUNCTION | "let" -> LET | _ -> IDENT
