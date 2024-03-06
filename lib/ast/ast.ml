(* Abstracting the Node behavior through a function type *)

type identifier_expression = {token: Token.token; value: string}

type expression = Identifier of identifier_expression

type let_statement =
  { token: Token.token
  ; name: identifier_expression (* Simplified for demonstration *)
  ; value: expression (* Assuming this is an expression node *) }

type statement = Letstatement of let_statement

type program =
  {statements: statement list (* List of nodes representing statements *)}

let token_literal_of_program p =
  match p with
  | [] ->
      ""
  | h :: _ -> (
    match h with Letstatement {value= _; name; token= _} -> name )
