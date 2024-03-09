(* Abstracting the Node behavior through a function type *)

type identifier_expression =
  {token: Token.token (* The ident token *); value: string}

type expression = Identifier of identifier_expression

type return_statement = {token: Token.token; return_value: expression}

type let_statement = {token: Token.token; name: identifier_expression}

type statement =
  | Letstatement of let_statement
  | Returnstatement of return_statement

let new_let_statement ?(token = {Token.type'= Token.EOF; Token.literal= "\x00"})
    ?(name =
      {token= {Token.type'= Token.EOF; Token.literal= "\x00"}; value= "\x00"})
    () =
  Letstatement {name; token}

let print_statement (s : statement) : unit =
  match s with
  | Letstatement {token} ->
      Format.printf "Token: %s\n" (Token.token_to_string_debug token.type')
  | Returnstatement {token} ->
      Format.printf "Token: %s\n" (Token.token_to_string_debug token.type')

let new_let_statement () =
  Letstatement
    { token= {type'= Token.EQ; literal= "="}
    ; name= {token= {type'= Token.EQ; literal= "="}; value= "useless"} }

(* Simplified for demonstration *)
(* ; value: expression (\* Assuming this is an expression node *\) } *)

type program =
  {statements: statement list (* List of nodes representing statements *)}

let token_literal_of_program p =
  match p with
  | [] ->
      ""
  | h :: _ -> (
    match h with
    | Letstatement {name; token= _} ->
        Token.token_to_string name.token.type'
    | Returnstatement {token} ->
        Token.token_to_string token.type' )
