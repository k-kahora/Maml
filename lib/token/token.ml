type token_type = string
type token = {
type' : token_type;
literal : string;
}

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
(* Keywords *)
| FUNCTION
| LET
