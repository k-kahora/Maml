(* Abstracting the Node behavior through a function type *)

type expression =
  | Identifier of ident
  | IntegerLiteral of {token: Token.token (* The ident token *); value: int}
  | StringLiteral of {token: Token.token (* The ident token *); value: string}
  | ArrayLiteral of
      {token: Token.token (* The ident token *); elements: expression list}
  | PrefixExpression of {token: Token.token; operator: string; right: expression}
  | InfixExpression of
      {token: Token.token; left: expression; operator: string; right: expression}
  | BooleanExpression of {token: Token.token; value: bool}
  | IfExpression of
      { token: Token.token
      ; condition: expression
      ; consquence: statement
      ; altenative: statement option }
  | FunctionLiteral of
      {token: Token.token; parameters: expression list; body: statement}
  | CallExpression of
      {token: Token.token; func: expression; arguments: expression list}
(* TODO IS there a way t restrict the type to a blockstatement *)

and statement =
  | Letstatement of {token: Token.token; name: expression; value: expression}
  | Returnstatement of {token: Token.token; return_value: expression}
  | Expressionstatement of {token: Token.token; expression: expression}
  | BlockStatement of block

and block = {token: Token.token; statements: statement list}

and ident = {token: Token.token (* The ident token *); value: string}

let expression_str_debug e =
  match e with
  | Identifier _ ->
      "Identifier"
  | StringLiteral _ ->
      "StringLiteral"
  | IntegerLiteral _ ->
      "IntegerLiteral"
  | PrefixExpression _ ->
      "PrefixExpression"
  | InfixExpression _ ->
      "InfixExpression"
  | BooleanExpression _ ->
      "BooleanExpression"
  | IfExpression _ ->
      "IfExpression"
  | FunctionLiteral _ ->
      "FunctionLiteral"
  | CallExpression _ ->
      "CallExpression"
  | ArrayLiteral _ ->
      "ArrayLiteral"

let rec expression_str (e : expression) : string =
  match e with
  | Identifier {token= _; value} ->
      value
  | StringLiteral {token= _; value} ->
      value
  | IntegerLiteral {token= _; value} ->
      string_of_int value
  | PrefixExpression {operator; right; _} ->
      "(" ^ operator ^ expression_str right ^ ")"
  | InfixExpression {left; operator; right; _} ->
      "(" ^ expression_str left ^ " " ^ operator ^ " " ^ expression_str right
      ^ ")"
  | BooleanExpression {token; _} ->
      token.literal
  | IfExpression {condition; consquence; altenative; _} -> (
      "if" ^ expression_str condition ^ " "
      ^ statement_str_helper consquence
      ^
      match altenative with
      | Some alt ->
          "else" ^ statement_str_helper alt
      | None ->
          "" )
  | FunctionLiteral {token; parameters; body} ->
      token.literal ^ "("
      ^ String.concat ", " (List.map (fun exp -> expression_str exp) parameters)
      ^ ")" ^ statement_str_helper body
  | CallExpression {arguments; func; _} ->
      expression_str func ^ "("
      ^ (String.concat ", " @@ List.map (fun a -> expression_str a) arguments)
      ^ ")"
  | ArrayLiteral {token= _; elements} ->
      Format.sprintf "[%s]"
        (List.map expression_str elements |> String.concat ", ")

(* {token: Token.token; parameters: ident list; body: statement} *)
(* TODO "if" ^ expression_str condition ^ " " ^ consequence *)
and statement_str_helper stat =
  match stat with
  | Letstatement {token; name; value} -> (
    match name with
    | Identifier {value= name_value; _} ->
        Token.token_to_string_debug token.type'
        ^ " " ^ name_value ^ " = " ^ expression_str value ^ ";"
    | _ ->
        failwith "Needs to be an identifier" )
  | Returnstatement {token; return_value} ->
      Token.token_to_string_debug token.type'
      ^ " "
      ^ expression_str return_value
      ^ ";"
  | Expressionstatement exp_stmt ->
      expression_str exp_stmt.expression
  | BlockStatement bl ->
      String.concat "\n"
      @@ List.map (fun acc -> statement_str_helper acc) bl.statements

let statement_str s = statement_str_helper s

let expr_str e = expression_str e

(* Simplified for demonstration *)
(* ; value: expression (\* Assuming this is an expression node *\) } *)

type program =
  {statements: statement list (* List of nodes representing statements *)}

let program_str (pgm : program) : string =
  let rec program_str_helper (acc : string) (list : statement list) : string =
    match list with
    | h :: t ->
        program_str_helper (acc ^ statement_str h) t
    | [] ->
        acc
  in
  program_str_helper "" pgm.statements

(* let token_literal_of_program p = *)
(*   match p with *)
(*   | [] -> *)
(*       "" *)
(*   | h :: _ -> ( *)
(*     match h with *)
(*     | Letstatement {name} -> *)
(*         Token.token_to_string name.token.type' *)
(*     | Returnstatement {token} -> *)
(*         Token.token_to_string token.type' *)
(*     | Expressionstatement _ -> *)
(*         failwith "TODO: Provide " ) *)
