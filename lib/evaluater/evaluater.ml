let true_object = Object.Bool true

let false_object = Object.Bool false

let null_object = Object.Null

let new_error fmt = Object.Error fmt

let is_truth b =
  match b with
  | Object.Bool boolean ->
      boolean
  | Object.Null ->
      false
  | _ ->
      true

(* looks for return statements for early returns *)
let eval_prefix operator right =
  let eval_minus_operator right =
    match right with
    | Object.Int b ->
        Object.Int (b * -1)
    | _ ->
        new_error
          (Format.sprintf "unknown operator: -%s" @@ Object.object_string right)
  in
  let eval_bang_operator right =
    match right with
    | Object.Bool b ->
        Object.Bool (not b)
    | Object.Null ->
        Object.Bool true
    | _ ->
        Object.Bool false
  in
  match operator with
  | "!" ->
      eval_bang_operator right
  | "-" ->
      eval_minus_operator right
  | a ->
      new_error
        (Format.sprintf "unknown operator: %s %s" a
           (Object.item_to_string right) )

let eval_infix left operator right =
  let eval_infix_bool_expression left_obj operator right_obj =
    (* this is risky but is handled in the calling function *)
    let Object.Bool left, Object.Bool right = (left_obj, right_obj) in
    match operator with
    | "==" ->
        Object.Bool (left == right)
    | "!=" ->
        Object.Bool (left != right)
    | _ ->
        new_error
        @@ (Format.sprintf "unknown operator: %s %s %s")
             (Object.object_string left_obj)
             operator
             (Object.object_string right_obj)
  in
  let eval_infix_expression left operator right =
    (* this is risky but is handled in the calling function *)
    let Object.Int left, Object.Int right = (left, right) in
    match operator with
    | "+" ->
        Object.Int (left + right)
    | "-" ->
        Object.Int (left - right)
    | "*" ->
        Object.Int (left * right)
    | "/" ->
        Object.Int (left / right)
    | ">" ->
        Object.Bool (left > right)
    | "<" ->
        Object.Bool (left < right)
    | "!=" ->
        Object.Bool (left <> right)
    | "==" ->
        Object.Bool (left = right)
    | a ->
        new_error (Format.sprintf "unknown operator: %s" a)
  in
  match (left, right) with
  | (Object.Int _ as l), (Object.Int _ as r) ->
      eval_infix_expression l operator r
  | (Object.Bool _ as l), (Object.Bool _ as r) ->
      eval_infix_bool_expression l operator r
  | Object.Bool _, Object.Int _ | Object.Int _, Object.Bool _ ->
      new_error
      @@ (Format.sprintf "type mismatch: %s %s %s")
           (Object.object_string left)
           operator
           (Object.object_string right)
  | _ ->
      failwith
        "No case detected for this scenario *eval_infix**eval_infix_expression"

(* let eval name = *)
(*   let open Ast in *)
let rec eval_expression = function
  | Ast.Identifier _ ->
      failwith "Identifier not yet implemented"
  | IntegerLiteral p ->
      Object.Int p.value
  | PrefixExpression p ->
      let right = eval_expression p.right in
      eval_prefix p.operator right
  | InfixExpression p ->
      let right = eval_expression p.right in
      let left = eval_expression p.left in
      eval_infix left p.operator right
  | BooleanExpression p ->
      if p.value then true_object else false_object
  | IfExpression p ->
      let condition = eval_expression p.condition in
      if is_truth condition then eval_statement p.consquence
      else Option.fold ~none:Object.Null ~some:eval_statement p.altenative
  | FunctionLiteral _ ->
      failwith "FunctionLiteral not yet implemented"
  | CallExpression _ ->
      failwith "CallExpression not yet implemented"

and eval_statement = function
  | Letstatement _ ->
      failwith "Let statement not yet implemented"
  | Returnstatement p ->
      let value = eval_expression p.return_value in
      Return value
  | Expressionstatement exp ->
      eval_expression exp.expression
  | BlockStatement block ->
      eval_block_statement block.statements
(* receives a statement list and evaluates the last statement*)

and eval_statements stmt_list =
  let rec eval_statements' acc stmt_list =
    match stmt_list with
    | _ when Object.is_return acc ->
        Object.unwrap_return acc
    | _ when Object.is_error acc ->
        acc
    | [] ->
        acc
    | Ast.Returnstatement stmt :: _ ->
        eval_expression stmt.return_value
    | h :: t ->
        eval_statements' (eval_statement h) t
  in
  (* print_endline @@ Ast.statement_str (List.hd stmt_list) ; *)
  eval_statements' Object.Null stmt_list

and eval_block_statement statements =
  let rec eval_block_statement' acc statements =
    match statements with
    | [] ->
        acc
    | _ when Object.is_error acc ->
        acc
    | _ when Object.is_return acc ->
        acc
    | h :: t ->
        eval_block_statement' (eval_statement h) t
  in
  eval_block_statement' Object.Null statements

let eval name =
  let open Ast in
  print_endline @@ program_str name ;
  eval_statements name.statements

(* TODO IS there a way t restrict the type to a blockstatement *)
