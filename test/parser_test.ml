let blank () = ()

let test_prefix_expression () =
  let prefix_tests = [("!5;", "!", 5); ("-15;", "-", 15)] in
  let test_inputs (input, operator, _value) =
    let l = Lexer.new' input in
    let p = Parser.new_parser l in
    let program = Parser.parse_program p in
    if List.length program.statements <> 1 then
      failwith
        ( "program.Statements does not contain enought statements got"
        ^ string_of_int
        @@ List.length program.statements ) ;
    match List.nth program.statements 0 with
    | Ast.Expressionstatement stm -> (
      match stm.expression with
      | Ast.PrefixExpression pe ->
          Alcotest.(check string) "Checking operator" pe.operator operator
      | _ ->
          failwith "needs to be a prefix expression" )
    | _ ->
        failwith "not an expression statement"
  in
  List.iter test_inputs prefix_tests

let test_int_literal () =
  let input = "5;" in
  let l = Lexer.new' input in
  let p = Parser.new_parser l in
  let program = Parser.parse_program p in
  if List.length program.statements <> 1 then
    failwith "should be one int literal" ;
  (* This is a type check *)
  let stmt =
    (* FIXME Redundent *)
    match program.statements with h :: _ -> h | _ -> failwith "impossible"
  in
  (* let _ = (stmt : Ast.expression_statement) in *)
  let value =
    match stmt with
    | Ast.Expressionstatement stm -> (
      match stm.expression with
      (* Must be i identified expression *)
      | IntegerLiteral int_expr ->
          int_expr.value
      | _ ->
          failwith "must be an integer expression" )
    | _ ->
        failwith "impossilbe"
    (* | _ -> *)
    (*     failwith "impossilbe" ) *)
  in
  if value <> 5 then failwith ("value not correct it is " ^ string_of_int value)

let test_ident_expression () =
  let input = "foobar;" in
  let l = Lexer.new' input in
  let p = Parser.new_parser l in
  let program = Parser.parse_program p in
  if List.length program.statements <> 1 then
    failwith "should be one expression" ;
  (* This is a type check *)
  let stmt =
    match program.statements with h :: _ -> h | _ -> failwith "impossible"
  in
  (* let _ = (stmt : Ast.expression_statement) in *)
  let token =
    match stmt with
    | Ast.Expressionstatement stm -> (
      match stm.expression with
      (* Must be i identified expression *)
      | Identifier ident_expr ->
          ident_expr.token
      | _ ->
          failwith "Must be an identfier expression" )
    | _ ->
        failwith "impossilbe"
    (* | _ -> *)
    (*     failwith "impossilbe" ) *)
  in
  if token.literal <> "foobar" then
    failwith ("value not correct it is " ^ token.literal)

let test_return_statements () =
  let input = {|
return 5; 
return 10; 
return 993322; 
|} in
  let l = Lexer.new' input in
  let p = Parser.new_parser l in
  let program = Parser.parse_program p in
  if List.length program.statements <> 3 then failwith "should be 3 statements" ;
  let test_inputs stat =
    match stat with
    | Ast.Returnstatement {token} ->
        Format.printf "True-Token: %s\n"
          (Token.token_to_string_debug token.type') ;
        Alcotest.(check string)
          "Check return statement"
          (Token.token_to_string_debug token.type')
          "RETURN"
    | _ ->
        failwith "should be a return statement"
  in
  List.iter test_inputs program.statements

let test_let_statement () =
  let input =
    {|
   let x = 5;
   let y = 10;
   let foobar = 838383;
   let special_ident = 10;
   |}
  in
  let tests = ["x"; "y"; "foobar"; "special_ident"] in
  let l = Lexer.new' input in
  let p = Parser.new_parser l in
  let program = Parser.parse_program p in
  let _ = print_endline (Ast.program_str program) in
  if List.length program.statements <> 4 then failwith "not enought statements" ;
  let test_inputs stat actual =
    match stat with
    | Ast.Letstatement {name} ->
        Alcotest.(check string) "Check name" name.value actual
    | _ ->
        failwith "Needs to be a let statement"
  in
  ignore (List.map2 test_inputs program.statements tests)

(* if List.length program.statements > 3 then *)
(*   failwith "To many statements produced" *)
(* else ignore 10 ignore 10 *)

let () =
  let open Alcotest in
  run "parser tests"
    [ ( "Let statements"
      , [test_case "Test the let statements" `Quick test_let_statement] )
    ; ( "Return statements"
      , [test_case "Test the return statements" `Quick test_return_statements]
      )
    ; ( "identifiers"
      , [ test_case "test the identifier expression statement" `Quick
            test_ident_expression (* test_ident_expression *) ] )
    ; ( "integers"
      , [test_case "test the integer expressions" `Quick test_int_literal] )
    ; ( "prefix operators"
      , [test_case "Test the operators values" `Quick test_prefix_expression] )
    ]
