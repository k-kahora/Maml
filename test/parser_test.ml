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
      ) ]
