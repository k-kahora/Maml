let blank () = ()

type generic = String of string | Int of int | Bool of bool

let test_ident (exp : Ast.expression) (value' : string) : bool =
  match exp with
  | Ast.Identifier {value; _} when value <> value' ->
      Alcotest.(check string) "Checking identifer value" value' value ;
      false
  | Ast.Identifier {token; _} when token.literal <> value' ->
      Alcotest.(check string) "Checking identifer literal" value' token.literal ;
      false
  | Ast.Identifier _ ->
      true
  | _ ->
      failwith "test_ident expects a ident ast"

let check_int_literal exp =
  match exp with
  | Ast.IntegerLiteral {value; _} ->
      value
  | _ ->
      failwith "Non integer expression found"

let test_int_literal (exp : Ast.expression) (value : int) : bool =
  match exp with
  | Ast.IntegerLiteral i when i.value <> value ->
      false
  | Ast.IntegerLiteral _ ->
      true
  | _ ->
      failwith "Expected int literal expression not this"

let test_bool_expression (exp : Ast.expression) (value : bool) : bool =
  match exp with
  | Ast.BooleanExpression bo when value <> bo.value ->
      ignore (Alcotest.fail "boolean value not equal") ;
      false
  | Ast.BooleanExpression bo when string_of_bool value <> bo.token.literal ->
      print_endline bo.token.literal ;
      print_endline @@ string_of_bool value ;
      Alcotest.(check string)
        "Token value check" (string_of_bool value) bo.token.literal ;
      false
  | Ast.BooleanExpression _ ->
      true
  | _ ->
      failwith "Not a boolean expression"

let test_literal_expressions (exp : Ast.expression) (expected : generic) : bool
    =
  match expected with
  | String str ->
      test_ident exp str
  | Int it ->
      test_int_literal exp it
  | Bool b ->
      test_bool_expression exp b

let test_infix_expressions (exp : Ast.expression) (left : generic)
    (operator : string) (right : generic) : unit =
  match exp with
  | Ast.InfixExpression infix ->
      Alcotest.(check bool)
        "Checking left expression" true
        (test_literal_expressions infix.left left) ;
      Alcotest.(check string) "Checking operator" operator infix.operator ;
      Alcotest.(check bool)
        "Checking operator"
        (test_literal_expressions infix.right right)
        true
  | _ ->
      failwith "Exp is not an infix experssion"

let bool_tests () =
  let tests = [("true", true); ("false", false)] in
  let helper (input, actual) =
    let l = Lexer.new' input in
    let p = Parser.new_parser l in
    let program = Parser.parse_program p in
    if List.length program.statements <> 1 then failwith "not enough statements" ;
    match List.hd program.statements with
    | Ast.Expressionstatement exp -> (
      match exp.expression with
      | Ast.BooleanExpression b ->
          Alcotest.(check bool) "Checking boolean values" actual b.value
      | _ ->
          failwith "should be a boolean value" )
    | _ ->
        failwith "should be an expression"
  in
  List.iter helper tests

let test_operator_precedence_parsing () =
  let precedence_test =
    [ ("false", "false")
    ; ("true", "true")
    ; ("3 > 5 == false", "((3 > 5) == false)")
    ; ("3 < 5 == true", "((3 < 5) == true)")
    ; ("a + b", "(a + b)")
    ; ("-a * b", "((-a) * b)")
    ; ("1 + 2 + 3;", "((1 + 2) + 3)")
    ; ("!-a", "(!(-a))")
    ; ("a + b + c", "((a + b) + c)")
    ; ("a + b - c", "((a + b) - c)")
    ; ("a * b * c", "((a * b) * c)")
    ; ("a * b / c", "((a * b) / c)")
    ; ("a + b / c", "(a + (b / c))")
    ; ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)")
    ; ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)")
    ; ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))")
    ; ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))")
    ; ("400 - 30 * 50 / 10; foo * bar", "(400 - ((30 * 50) / 10))(foo * bar)")
    ; ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))")
      (* Grouped experssions *)
    ; ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)")
    ; ("(5 + 5) * 2", "((5 + 5) * 2)")
    ; ("10 * (3 + 2)", "(10 * (3 + 2))")
    ; ("2 / (5 + 5)", "(2 / (5 + 5))")
    ; ("-(5 + 5)", "(-(5 + 5))")
    ; ("!(true == true)", "(!(true == true))")
    ; ("a + add(b * c) + d", "((a + add((b * c))) + d)")
    ; ( "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))"
      , "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))" )
    ; ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))")
    ; ("a * [1,2,3,4][b * c] * d", "((a * ([1, 2, 3, 4][(b * c)])) * d)")
    ; ( "add(a * b[2], b[1], 2 * [1,2][1])"
      , "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))" ) ]
  in
  let helper (input, actual) =
    let l = Lexer.new' input in
    let p = Parser.new_parser l in
    let program = Parser.parse_program p in
    let true_val = Ast.program_str program in
    Alcotest.(check string "Checking large infix expressions" actual true_val)
  in
  List.iter helper precedence_test

let test_parsing_infix_expressions () =
  let infix_tests =
    [ ("5 + 5", Int 5, "+", Int 5)
    ; ("5 - 5", Int 5, "-", Int 5)
    ; ("5 * 5", Int 5, "*", Int 5)
    ; ("5 / 5", Int 5, "/", Int 5)
    ; ("5 > 5", Int 5, ">", Int 5)
    ; ("5 < 5", Int 5, "<", Int 5)
    ; ("5 == 5", Int 5, "==", Int 5)
    ; ("5 != 5", Int 5, "!=", Int 5)
    ; ("true == true", Bool true, "==", Bool true)
    ; ("true != false", Bool true, "!=", Bool false)
    ; ("false == false", Bool false, "==", Bool false) ]
    (* ; ("true == true", Bool true, "==", Bool true) ] *)
  in
  let test_infix_helper (input, left_value, operator, right_value) =
    let l = Lexer.new' input in
    let p = Parser.new_parser l in
    let program = Parser.parse_program p in
    match List.nth_opt program.statements 0 with
    | Some exp -> (
      (* Expression statement matching *)
      match exp with
      | Ast.Expressionstatement stmt ->
          (* Expression type checking *)
          test_infix_expressions stmt.expression left_value operator right_value
      | _ ->
          failwith "not an expression statement" )
    | None ->
        failwith (* FIXME Error message is incorrect *)
          ( "Incorrect number of statements found"
          ^ (string_of_int @@ List.length program.statements)
          ^ "should be one" )
  in
  List.iter test_infix_helper infix_tests

let test_prefix_expressions () =
  let prefix_tests =
    [ ("!6456456;", "!", Int 6456456)
    ; ("-15;", "-", Int 15)
    ; ("!foobar;", "!", String "foobar")
    ; ("-ohmygod;", "-", String "ohmygod")
    ; ("-false;", "-", Bool false)
    ; ("!false;", "!", Bool false)
    ; ("-true;", "-", Bool true) ]
  in
  let test_inputs (input, operator, value) =
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
          Alcotest.(check string) "Testing operator" operator pe.operator ;
          Alcotest.(check bool)
            "Testing prefix expressions" true
            (test_literal_expressions pe.right value)
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

let test_return_statement () =
  let open Ast in
  let test_return_statement (l : Ast.statement) (expression_value : generic) =
    match l with
    | Ast.Returnstatement {token; return_value} ->
        (* Alcotest.(check string) "Checking let token" "let" token.literal ; *)
        Alcotest.(check string)
          "checking return statement token" "return" token.literal ;
        Alcotest.(check bool)
          "checking return statement" true
          (test_literal_expressions return_value expression_value)
    | _ ->
        failwith "should be a let statement"
  in
  let inputs =
    [ ("return 5;", Int 5)
    ; ("return 10", Int 10)
    ; ("return 4565460;", Int 4565460) ]
  in
  let f (input, expectedValue) =
    let l = Lexer.new' input in
    let p = Parser.new_parser l in
    let program = Parser.parse_program p in
    Alcotest.(check int) "length" 1 (List.length program.statements) ;
    test_return_statement (List.nth program.statements 0) expectedValue ;
    ()
  in
  List.iter f inputs

(* if List.length program.statements > 3 then *)
(*   failwith "To many statements produced" *)
(* else ignore 10 ignore 10 *)

let test_let_statement () =
  let open Ast in
  let test_let_statement (l : Ast.statement) (expected_name : string)
      (expression_value : generic) =
    match l with
    | Ast.Letstatement {name; value; token= _token} -> (
        (* Alcotest.(check string) "Checking let token" "let" token.literal ; *)
        Alcotest.(check bool)
          "checking let statement value" true
          (test_literal_expressions value expression_value) ;
        match name with
        | Identifier {value; token} ->
            Alcotest.(check string) "Checking ident" expected_name value ;
            Alcotest.(check string)
              "Checking ident token" expected_name token.literal
        | _ ->
            failwith "expressioin shoulb be an ident" )
    | _ ->
        failwith "should be a let statement"
  in
  let inputs =
    [ ("let x = 5;", "x", Int 5)
    ; ("let y = true;", "y", Bool true)
    ; ("let foobar = y;", "foobar", String "y") ]
  in
  let f (input, expectedIdent, expectedValue) =
    let l = Lexer.new' input in
    let p = Parser.new_parser l in
    let program = Parser.parse_program p in
    Alcotest.(check int) "length" 1 (List.length program.statements) ;
    test_let_statement
      (List.nth program.statements 0)
      expectedIdent expectedValue ;
    ()
  in
  List.iter f inputs

(* if List.length program.statements > 3 then *)
(*   failwith "To many statements produced" *)
(* else ignore 10 ignore 10 *)

let test_if_expression () =
  let test_block_statement (s : Ast.statement) : unit =
    let open Ast in
    match s with
    | BlockStatement {statements; _} -> (
        (* NOTE this test is not very general*)
        if List.length statements <> 1 then
          Fmt.failwith
            "consequence statements does not contain %d statement got %d\n" 1
            (List.length statements) ;
        match List.nth statements 0 with
        | Expressionstatement exp ->
            Alcotest.(check bool) "Checking ident in block" true
            @@ test_ident exp.expression "x"
        | _ ->
            failwith "needs to be an expression statement" )
    | _ ->
        failwith
          "not a block statement if expressions can only contain block \
           statements"
  in
  let input = "if (x < y) {x}" in
  let statements =
    Lexer.new' input |> Parser.new_parser |> Parser.parse_program
    |> fun a -> a.statements
  in
  if List.length statements <> 1 then
    Fmt.failwith "program statements does not contain %d statement got %d\n" 1
    @@ List.length statements ;
  let stmt = List.nth_opt statements 0 in
  match stmt with
  | Some st -> (
    match st with
    | Ast.Expressionstatement exp -> (
      match exp.expression with
      | Ast.IfExpression {token= _token; condition; consquence; altenative} -> (
          test_infix_expressions condition (String "x") "<" (String "y") ;
          test_block_statement consquence ;
          match altenative with
          | None ->
              ()
          | Some _ ->
              failwith "There should be no alternertive statement block" )
      | _ ->
          failwith "needs to be an IfExpression" )
    | _ ->
        failwith "Needs to be an expression statement" )
  | None ->
      failwith "statement list is empty"

let test_if_else_expression () =
  let test_block_statement (s : Ast.statement) (ident : string) : unit =
    let open Ast in
    match s with
    | BlockStatement {statements; _} -> (
        (* NOTE this test is not very general*)
        if List.length statements <> 1 then
          Fmt.failwith
            "consequence statements does not contain %d statement got %d\n" 1
            (List.length statements) ;
        match List.nth statements 0 with
        | Expressionstatement exp ->
            Alcotest.(check bool) "Checking ident in block" true
            @@ test_ident exp.expression ident
        | _ ->
            failwith "needs to be an expression statement" )
    | _ ->
        failwith
          "not a block statement if expressions can only contain block \
           statements"
  in
  let input = "if (x < y) {x} else {y}" in
  let statements =
    Lexer.new' input |> Parser.new_parser |> Parser.parse_program
    |> fun a -> a.statements
  in
  if List.length statements <> 1 then
    Fmt.failwith "program statements does not contain %d statement got %d\n" 1
    @@ List.length statements ;
  let stmt = List.nth_opt statements 0 in
  match stmt with
  | Some st -> (
    match st with
    | Ast.Expressionstatement exp -> (
      match exp.expression with
      | Ast.IfExpression {token= _token; condition; consquence; altenative} -> (
          test_infix_expressions condition (String "x") "<" (String "y") ;
          test_block_statement consquence "x" ;
          match altenative with
          | None ->
              ()
          | Some alt ->
              test_block_statement alt "y" )
      | _ ->
          failwith "needs to be an IfExpression" )
    | _ ->
        failwith "Needs to be an expression statement" )
  | None ->
      failwith "statement list is empty"

let test_call_expression_parsing () =
  let open Ast in
  let input = "add(1, 2 * 3, 4 + 5);" in
  let statements =
    Lexer.new' input |> Parser.new_parser |> Parser.parse_program
    |> fun a -> a.statements
  in
  if List.length statements <> 1 then
    Fmt.failwith "program statements does not contain %d statement got %d\n" 1
    @@ List.length statements ;
  let stmt = List.nth_opt statements 0 in
  match stmt with
  | Some st -> (
    match st with
    | Expressionstatement exp -> (
      match exp.expression with
      | CallExpression {func; arguments; _} ->
          Alcotest.(check bool) "func ident" true (test_ident func "add") ;
          Alcotest.(check int) "Argument list length" 3 (List.length arguments) ;
          Alcotest.(check bool)
            "arg 1" true
            (test_literal_expressions (List.nth arguments 0) (Int 1)) ;
          test_infix_expressions (List.nth arguments 1) (Int 2) "*" (Int 3) ;
          test_infix_expressions (List.nth arguments 2) (Int 4) "+" (Int 5)
      | _ ->
          failwith "not a function literal" )
    | _ ->
        failwith "not an expression statement" )
  | None ->
      failwith "not statements found"

let test_call_parameter_parsing () =
  let tests =
    [ ("add()", "add", [])
    ; ("add(1)", "add", ["1"])
    ; ("add(1,2*3,4 + 5)", "add", ["1"; "(2 * 3)"; "(4 + 5)"]) ]
  in
  (* let statements = *)
  (*   Lexer.new' input |> Parser.new_parser |> Parser.parse_program *)
  (*   |> fun a -> a.statements *)
  (* in *)
  let f (input, ident, p_list) =
    let statements =
      Lexer.new' input |> Parser.new_parser |> Parser.parse_program
      |> fun a -> a.statements
    in
    let extract_val = function
      | Ast.Identifier {value; _} ->
          value
      | _ ->
          "fail"
    in
    let stmt = List.nth_opt statements 0 in
    let open Ast in
    match stmt with
    | Some s -> (
      match s with
      | Expressionstatement exp -> (
        match exp.expression with
        | CallExpression {func; arguments; _} ->
            Alcotest.(check int)
              "argument list length" (List.length p_list)
              (List.length arguments) ;
            Alcotest.(check string) "checking ident" ident (extract_val func) ;
            List.iter2
              (fun expected actual ->
                Alcotest.(check string)
                  "checking parameter list" expected (expression_str actual) )
              p_list arguments
        | _ ->
            failwith "not a function literal" )
      | _ ->
          failwith "needs to be an expression statement" )
    | None ->
        failwith "statement list is empty"
  in
  List.iter f tests

let test_func_literal_parsing () =
  let open Ast in
  let input = "fn(x, y) {x + y}" in
  let statements =
    Lexer.new' input |> Parser.new_parser |> Parser.parse_program
    |> fun a -> a.statements
  in
  if List.length statements <> 1 then
    Fmt.failwith "program statements does not contain %d statement got %d\n" 1
    @@ List.length statements ;
  let stmt = List.nth_opt statements 0 in
  match stmt with
  | Some st -> (
    match st with
    | Expressionstatement exp -> (
      match exp.expression with
      | FunctionLiteral {parameters; body; token= _token} -> (
          Alcotest.(check int)
            "Checking parameter list" 2 (List.length parameters) ;
          Alcotest.(check bool) "Testing parameter one" true
          @@ test_literal_expressions (List.nth parameters 0)
          @@ String "x" ;
          Alcotest.(check bool) "Testing parameter one" true
          @@ test_literal_expressions (List.nth parameters 1)
          @@ String "y" ;
          let block =
            match body with
            | BlockStatement block ->
                block
            | _ ->
                failwith "body must be a block statement"
          in
          Alcotest.(check int)
            "Checking block statement list" 1
            (List.length block.statements) ;
          match List.nth block.statements 0 with
          | Expressionstatement exp ->
              test_infix_expressions exp.expression (String "x") "+" (String "y")
          | _ ->
              failwith "block body is not an expression statement" )
      | _ ->
          failwith "not a function literal" )
    | _ ->
        failwith "not an expression statement" )
  | None ->
      failwith "not statements found"

let test_function_parameter_passing () =
  let tests =
    [("fn() {};", []); ("fn(x) {x};", ["x"]); ("fn(x,y,z) {};", ["x"; "y"; "z"])]
  in
  (* let statements = *)
  (*   Lexer.new' input |> Parser.new_parser |> Parser.parse_program *)
  (*   |> fun a -> a.statements *)
  (* in *)
  let f (input, p_list) =
    let statements =
      Lexer.new' input |> Parser.new_parser |> Parser.parse_program
      |> fun a -> a.statements
    in
    let stmt = List.nth_opt statements 0 in
    let open Ast in
    match stmt with
    | Some s -> (
      match s with
      | Expressionstatement exp -> (
        match exp.expression with
        | FunctionLiteral fn ->
            Alcotest.(check int)
              "parameter list length" (List.length p_list)
              (List.length fn.parameters) ;
            List.iter2
              (fun expected actual ->
                Alcotest.(check bool)
                  "checking parameter list" true
                  (test_literal_expressions actual @@ String expected) )
              p_list fn.parameters
        | _ ->
            failwith "not a function literal" )
      | _ ->
          failwith "needs to be an expression statement" )
    | None ->
        failwith "statement list is empty"
  in
  List.iter f tests

let test_string_literal () =
  let input = "\"hello world\"" in
  let statements =
    Lexer.new' input |> Parser.new_parser |> Parser.parse_program
    |> fun a -> a.statements
  in
  let stmt = List.nth_opt statements 0 in
  match stmt with
  | Some st -> (
    match st with
    | Ast.Expressionstatement exp -> (
      match exp.expression with
      | Ast.StringLiteral str ->
          Alcotest.(check string)
            "Checking string literal" "hello world" str.value
      | p ->
          failwith ("expected string literal got " ^ Ast.expression_str_debug p)
      )
    | _ ->
        failwith "" )
  | None ->
      failwith "need at least one statement"

let test_array_literal () =
  let input = "[1,2 * 2, 3 + 3]" in
  let statements =
    Lexer.new' input |> Parser.new_parser |> Parser.parse_program
    |> fun a -> a.statements
  in
  match List.nth statements 0 with
  | Ast.Expressionstatement {token= _; expression} -> (
    match expression with
    | Ast.ArrayLiteral {token= _; elements= array} ->
        Alcotest.(check int)
          "First element" 1
          (List.nth array 0 |> check_int_literal) ;
        test_infix_expressions (List.nth array 1) (Int 2) "*" (Int 2) ;
        test_infix_expressions (List.nth array 2) (Int 3) "+" (Int 3)
    | _ ->
        failwith "needs to be an array" )
  | _ ->
      failwith "need at least one statement"

let test_empty_hash_literal () =
  let input = {|{}|} in
  let statements =
    Lexer.new' input |> Parser.new_parser |> Parser.parse_program
    |> fun a -> a.statements
  in
  match List.nth statements 0 with
  | Ast.Expressionstatement {token= _; expression} -> (
    match expression with
    | Ast.HashLiteral {token= _; pairs} ->
        Alcotest.(check int) "Hash table length of empty hash" 0
        @@ Hashtbl.length pairs
    | _ ->
        failwith "needs to be an array" )
  | _ ->
      failwith "need at least one statement"

(* This test is horrible refactor so it does not have so mony doomed scenarios *)
let test_hash_literal () =
  let input = {|{"one": 1, "two": 2, "three": 3}|} in
  let statements =
    Lexer.new' input |> Parser.new_parser |> Parser.parse_program
    |> fun a -> a.statements
  in
  let open Hashtbl in
  let test_hash = create 3 in
  add test_hash "one" 1 ;
  add test_hash "two" 2 ;
  add test_hash "three" 3 ;
  match List.nth statements 0 with
  | Ast.Expressionstatement {token= _; expression} -> (
    match expression with
    | Ast.HashLiteral {token= _; pairs} ->
        Hashtbl.iter
          (fun key value ->
            Alcotest.(check int)
              "Hash check for one"
              (Hashtbl.find test_hash (Ast.expression_str key))
              (Ast.expression_str value |> int_of_string) )
          pairs
    | _ ->
        failwith "needs to be an array" )
  | _ ->
      failwith "need at least one statement"

(* This test is horrible refactor so it does not have so mony doomed scenarios *)
let test_hash_infix_literal () =
  let input = {|{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}|} in
  let statements =
    Lexer.new' input |> Parser.new_parser |> Parser.parse_program
    |> fun a -> a.statements
  in
  let open Hashtbl in
  let test_hash = create 3 in
  add test_hash "one" (fun e -> test_infix_expressions e (Int 0) "+" (Int 1)) ;
  add test_hash "two" (fun e -> test_infix_expressions e (Int 10) "-" (Int 8)) ;
  add test_hash "three" (fun e -> test_infix_expressions e (Int 15) "/" (Int 5)) ;
  match List.nth statements 0 with
  | Ast.Expressionstatement {token= _; expression} -> (
    match expression with
    | Ast.HashLiteral {token= _; pairs} ->
        Hashtbl.iter
          (fun key value ->
            Hashtbl.find test_hash (Ast.expr_str key) |> fun a -> a value )
          pairs
    | _ ->
        failwith "needs to be an array" )
  | _ ->
      failwith "need at least one statement"

let () =
  let open Alcotest in
  run "parser tests"
    [ ( "Let statements"
      , [test_case "Test the let statements" `Quick test_let_statement] )
    ; ( "Return statements"
      , [test_case "Test the return statements" `Quick test_return_statement] )
    ; ( "identifiers"
      , [ test_case "test the identifier expression statement" `Quick
            test_ident_expression (* test_ident_expression *) ] )
    ; ( "integers"
      , [test_case "test the integer expressions" `Quick test_int_literal] )
    ; ( "prefix operators"
      , [test_case "Test the operators values" `Quick test_prefix_expressions]
      )
    ; ( "infix operators"
      , [ test_case "Test the infix operators values" `Quick
            test_parsing_infix_expressions ] )
    ; ( "infix operators precedence"
      , [ test_case "Test the precendenc operators values" `Quick
            test_operator_precedence_parsing ] )
    ; ( "Testing boolean expressions"
      , [test_case "boolean expressions" `Quick bool_tests] )
    ; ( "Testing if expressions"
      , [test_case "if expression" `Quick test_if_expression] )
    ; ( "Test if else expression"
      , [test_case "if else expression" `Quick test_if_else_expression] )
    ; ( "Function literals"
      , [test_case "functions" `Quick test_func_literal_parsing] )
    ; ( "call expressions"
      , [test_case "call expressions" `Quick test_call_expression_parsing] )
    ; ( "string hello world test"
      , [test_case "string expressions" `Quick test_string_literal] )
    ; ( "test parsing array literal"
      , [test_case "array literal to string" `Quick test_array_literal] )
    ; ( "test parsing hash literal"
      , [test_case "hash literal to string" `Quick test_hash_literal] )
    ; ( "test parsing empty hash literal"
      , [test_case "empty hash literal to string" `Quick test_empty_hash_literal]
      )
    ; ( "call expressions arguments"
      , [ test_case "call expressions arguments" `Quick
            test_call_parameter_parsing ] ) ]
