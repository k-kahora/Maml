open Object

let true_object = Obj.Bool true

let false_object = Obj.Bool false

let null_object = Obj.Null

let new_error fmt = Obj.Error fmt

let is_truth b =
  match b with Obj.Bool boolean -> boolean | Obj.Null -> false | _ -> true

(* looks for return statements for early returns *)
let eval_prefix operator right =
  let eval_minus_operator right =
    match right with
    | Obj.Int b ->
        Obj.Int (b * -1)
    | _ ->
        new_error
          (Format.sprintf "unknown operator: -%s" @@ Obj.object_string right)
  in
  let eval_bang_operator right =
    match right with
    | Obj.Bool b ->
        Obj.Bool (not b)
    | Obj.Null ->
        Obj.Bool true
    | _ ->
        Obj.Bool false
  in
  match operator with
  | "!" ->
      eval_bang_operator right
  | "-" ->
      eval_minus_operator right
  | a ->
      new_error
        (Format.sprintf "unknown operator: %s %s" a (Obj.item_to_string right))

let eval_infix left operator right =
  let eval_infix_bool_expression left_obj operator right_obj =
    (* this is risky but is handled in the calling function *)
    let Obj.Bool left, Obj.Bool right = (left_obj, right_obj) in
    match operator with
    | "==" ->
        Obj.Bool (left == right)
    | "!=" ->
        Obj.Bool (left != right)
    | _ ->
        new_error
        @@ (Format.sprintf "unknown operator: %s %s %s")
             (Obj.object_string left_obj)
             operator
             (Obj.object_string right_obj)
  in
  let eval_infix_expression left operator right =
    (* this is risky but is handled in the calling function *)
    let Obj.Int left, Obj.Int right = (left, right) in
    match operator with
    | "+" ->
        Obj.Int (left + right)
    | "-" ->
        Obj.Int (left - right)
    | "*" ->
        Obj.Int (left * right)
    | "/" ->
        Obj.Int (left / right)
    | ">" ->
        Obj.Bool (left > right)
    | "<" ->
        Obj.Bool (left < right)
    | "!=" ->
        Obj.Bool (left <> right)
    | "==" ->
        Obj.Bool (left = right)
    | a ->
        new_error (Format.sprintf "unknown operator: %s" a)
  in
  match (left, right) with
  | (Obj.Int _ as l), (Obj.Int _ as r) ->
      eval_infix_expression l operator r
  | (Obj.Bool _ as l), (Obj.Bool _ as r) ->
      eval_infix_bool_expression l operator r
  | Obj.Bool _, Obj.Int _ | Obj.Int _, Obj.Bool _ ->
      new_error
      @@ (Format.sprintf "type mismatch: %s %s %s")
           (Obj.object_string left) operator (Obj.object_string right)
  | _ ->
      failwith
        "No case detected for this scenario *eval_infix**eval_infix_expression"

(* let eval name = *)
(*   let open Ast in *)
let eval_ident env value = Environment.get env value

let rec eval_expression (env : Environment.environment) = function
  | Ast.Identifier {value} ->
      eval_ident env value
  | IntegerLiteral p ->
      Obj.Int p.value
  | PrefixExpression p ->
      let open Object.Obj in
      let right = eval_expression env p.right in
      if is_error right then right else eval_prefix p.operator right
  | InfixExpression p ->
      let open Object.Obj in
      let right = eval_expression env p.right in
      let left = eval_expression env p.left in
      if is_error left then left
      else if is_error right then right
      else eval_infix left p.operator right
  | BooleanExpression p ->
      if p.value then true_object else false_object
  | IfExpression p ->
      let open Object.Obj in
      let condition = eval_expression env p.condition in
      if is_error condition then condition
      else if is_truth condition then eval_statement env p.consquence
      else Option.fold ~none:Obj.Null ~some:(eval_statement env) p.altenative
  | FunctionLiteral {parameters; body} ->
      Obj.Function (parameters, body, env)
  | CallExpression {func; arguments} -> (
      let func = eval_expression env func in
      match func with
      | Object.Obj.Error _ ->
          func
      | _ ->
          let args = eval_expressions env arguments in
          if List.hd args = Obj.Null then List.hd args
          else apply_function func args )

and eval_statement (env : Environment.environment) = function
  | Letstatement p ->
      let value = eval_expression env p.value in
      if Obj.is_error value then value
      else
        let env_new, _v =
          (* FIXME why the hell does set return a tuple you only need te env *)
          Environment.set env
            ( match p.name with
            | Ast.Identifier {value= name} ->
                name
            | _ ->
                failwith "LetStatement does not have an ident *weird error*" )
            value
        in
        env.store <- env_new.store ;
        value
  | Returnstatement p ->
      let value = eval_expression env p.return_value in
      let open Object.Obj in
      if is_error value then value else Return value
  | Expressionstatement exp ->
      eval_expression env exp.expression
  | BlockStatement block ->
      eval_block_statement env block.statements
(* receives a statement list and evaluates the last statement*)

and eval_statements env stmt_list =
  let rec eval_statements' acc stmt_list =
    match stmt_list with
    | _ when Obj.is_return acc ->
        Obj.unwrap_return acc
    | _ when Obj.is_error acc ->
        acc
    | [] ->
        acc
    | Ast.Returnstatement stmt :: _ ->
        eval_expression env stmt.return_value
    | h :: t ->
        eval_statements' (eval_statement env h) t
  in
  (* print_endline @@ Ast.statement_str (List.hd stmt_list) ; *)
  eval_statements' Obj.Null stmt_list

and eval_block_statement env statements =
  let rec eval_block_statement' acc statements =
    match statements with
    | [] ->
        acc
    | _ when Obj.is_error acc ->
        acc
    | _ when Obj.is_return acc ->
        acc
    | h :: t ->
        eval_block_statement' (eval_statement env h) t
  in
  eval_block_statement' Obj.Null statements

(* Turns each expression into an object or one error object if there is an error *)
and eval_expressions env exps =
  let params = List.map (fun a -> eval_expression env a) exps in
  match List.find_opt Obj.is_error params with None -> params | Some a -> [a]

and apply_function (func : Obj.item) (args : Obj.item list) =
  let fold_left_with_index f init lst =
    let rec aux i acc = function
      | [] ->
          acc
      | x :: xs ->
          aux (i + 1) (f i acc x) xs
    in
    aux 0 init lst
  in
  let extract_parameter exp =
    match exp with
    | Ast.Identifier {value} ->
        value
    | _ ->
        failwith "imposibble"
  in
  let extend_function_env fn =
    match fn with
    | Obj.Function (params, _, env) ->
        fold_left_with_index
          (fun i acc nxt ->
            fst (Environment.set acc (extract_parameter nxt) (List.nth args i))
            )
          (Environment.new_enclosed_environment env)
          params
    | _ ->
        failwith ""
    (* let env = Environment.new_enclosed_environment env in *)
    (* List.fold_left (fun acc nxt -> Environment.set acc  ) params  *)
  in
  let unwrap_return_value obj = match obj with Obj.Return a -> a | _ -> obj in
  match func with
  | Obj.Function _ ->
      let extended_env = extend_function_env func in
      let evaluated =
        match func with
        | Obj.Function (_, stmt, _) ->
            eval_statement extended_env stmt
        | _ ->
            failwith ""
      in
      unwrap_return_value evaluated
  | _ ->
      Obj.Error "not a function"

let eval (env : Environment.environment) name =
  let open Ast in
  print_endline @@ program_str name ;
  eval_statements env name.statements

(* TODO IS there a way t restrict the type to a blockstatement *)
