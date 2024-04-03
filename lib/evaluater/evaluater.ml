let true_object = Object.Bool true

let false_object = Object.Bool false

let null_object = Object.Null

let is_truth b =
  match b with
  | Object.Bool boolean ->
      boolean
  | Object.Null ->
      false
  | _ ->
      true

let eval name =
  let open Ast in
  let rec eval_expression = function
    | Identifier _ ->
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
        if is_truth condition then eval_helper p.consquence
        else Option.fold ~none:Object.Null ~some:eval_helper p.altenative
    | FunctionLiteral _ ->
        failwith "FunctionLiteral not yet implemented"
    | CallExpression _ ->
        failwith "CallExpression not yet implemented"
  and eval_prefix operator right =
    let eval_minus_operator right =
      match right with Object.Int b -> Object.Int (b * -1) | _ -> Object.Null
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
    | _ ->
        failwith "only two operators shoulb be possible"
  and eval_infix left operator right =
    let eval_infix_bool_expression left operator right =
      match operator with
      | "==" ->
          Object.Bool (left == right)
      | "!=" ->
          Object.Bool (left != right)
      | _ ->
          Object.Null
    in
    let eval_infix_expression left operator right =
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
      | _ ->
          Object.Null
    in
    match (left, right) with
    | Object.Int l, Object.Int r ->
        eval_infix_expression l operator r
    | Object.Bool l, Object.Bool r ->
        eval_infix_bool_expression l operator r
    | _ ->
        Object.Null
  and eval_helper = function
    | Letstatement _ ->
        failwith "Let statement not yet implemented"
    | Returnstatement p ->
        let value = eval_expression p.return_value in
        value
    | Expressionstatement exp ->
        eval_expression exp.expression
    | BlockStatement block ->
        eval_statements block.statements
  (* receives a statement list and evaluates the last statement*)
  and eval_statements stmt_list =
    match stmt_list with
    | [] ->
        failwith "list is empty"
    | Ast.Returnstatement stmt :: _ ->
        eval_expression stmt.return_value
    | [x] ->
        eval_helper x
    | _ :: t ->
        eval_statements t
  in
  eval_statements name.statements

(* TODO IS there a way t restrict the type to a blockstatement *)
