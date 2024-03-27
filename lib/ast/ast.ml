(* Abstracting the Node behavior through a function type *)

type expression =
  | Identifier of ident
  | IntegerLiteral of {token: Token.token (* The ident token *); value: int}
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
      {token: Token.token; parameters: ident list; body: statement}
(* TODO IS there a way t restrict the type to a blockstatement *)

and statement =
  | Letstatement of {token: Token.token; name: expression; value: expression}
  | Returnstatement of {token: Token.token; return_value: expression}
  | Expressionstatement of {token: Token.token; expression: expression}
  | BlockStatement of block

and block = {token: Token.token; statements: statement list}

and ident = {token: Token.token (* The ident token *); value: string}

let ast_to_str a =
  let rec expression_str (e : expression) : string =
    match e with
    | Identifier {token= _; value} ->
        value
    | IntegerLiteral {token= _; value} ->
        string_of_int value
    | PrefixExpression {operator; right} ->
        "(" ^ operator ^ expression_str right ^ ")"
    | InfixExpression {left; operator; right} ->
        "(" ^ expression_str left ^ " " ^ operator ^ " " ^ expression_str right
        ^ ")"
    | BooleanExpression {token} ->
        token.literal
    | IfExpression {condition; consquence; altenative} -> (
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
        ^ List.fold_left (fun acc {value} -> value ^ acc) "" parameters
        ^ ")" ^ statement_str_helper body
  (* {token: Token.token; parameters: ident list; body: statement} *)
  (* TODO "if" ^ expression_str condition ^ " " ^ consequence *)
  and statement_str_helper stat =
    match stat with
    | Letstatement {token; name; value} -> (
      match name with
      | Identifier {value= name_value} ->
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
        List.fold_left
          (fun acc nxt -> acc ^ statement_str_helper nxt)
          "" bl.statements
  in
  statement_str_helper a

let statement_str s = ast_to_str s

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
