open Compiler

let parse input =
  Lex.new' input |> Parsing.new_parser |> Parsing.parse_program
  |> fun a -> a.statements

open Code

let test_iter = List.iter

let test_instructions expected actual =
  let expected = List.concat expected in
  Alcotest.(check int)
    "Instruction lengths" (List.length expected) (List.length actual) ;
  Alcotest.(check (list char)) "Actual instruction values" expected actual

let test_constants _ _ = ()

let run_compiler_tests tests =
  let helper (input, expected_constants, expected_instructions) =
    let program = parse input in
    let compiler = new_compiler in
    let _errors = compile compiler program in
    let bytecode = bytecode compiler in
    test_instructions expected_instructions bytecode.instructions' ;
    test_constants expected_constants bytecode.constants'
  in
  test_iter helper tests

let test_int_arithmetic () =
  let tests =
    [("1 + 2", [1; 2], [make @@ `OpConstant 0; make @@ `OpConstant 1])]
  in
  run_compiler_tests tests

let () =
  Alcotest.run "OpConstant arithmetic checking"
    [ ( "testing compiler"
      , [Alcotest.test_case "int arithmetic" `Quick test_int_arithmetic] ) ]
