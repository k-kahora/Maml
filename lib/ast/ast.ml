(* Abstracting the Node behavior through a function type *)

type identifier_expression =
  {token: Token.token (* The ident token *); value: string}

type expression = Identifier of identifier_expression

type return_statement = {token: Token.token; return_value: expression}

type let_statement =
  {token: Token.token; name: identifier_expression; value: expression}

let expression_str (e : expression) : string =
  match e with Identifier {token= _; value} -> value

let identifier_str {value} = value

let let_statement_str {token; name; value} =
  Token.token_to_string_debug token.type'
  ^ " " ^ name.value ^ " = " ^ expression_str value ^ ";"

let return_statement_str {token; return_value} =
  Token.token_to_string_debug token.type'
  ^ " "
  ^ expression_str return_value
  ^ ";"

type expression_statement = {token: Token.token; expression: expression}

type statement =
  | Letstatement of let_statement
  | Returnstatement of return_statement
  | Expressionstatement of expression_statement

let statement_str (s : statement) : string =
  match s with
  | Letstatement let_stmt ->
      let_statement_str let_stmt
  | Returnstatement return_stmt ->
      return_statement_str return_stmt
  | _ ->
      "FILLER FOR EXPRESSION STMT"

(* Simplified for demonstration *)
(* ; value: expression (\* Assuming this is an expression node *\) } *)

type program =
  {statements: statement list (* List of nodes representing statements *)}

let program_str (pgm : program) : string =
  let rec program_str_helper (acc : string) (list : statement list) : string =
    match list with
    | h :: t ->
        program_str_helper (statement_str h ^ acc) t
    | [] ->
        acc
  in
  program_str_helper "" pgm.statements

let token_literal_of_program p =
  match p with
  | [] ->
      ""
  | h :: _ -> (
    match h with
    | Letstatement {name} ->
        Token.token_to_string name.token.type'
    | Returnstatement {token} ->
        Token.token_to_string token.type'
    | Expressionstatement _ ->
        failwith "TODO: Provide " )
