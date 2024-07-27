open Object
open Code

let nil a = a

type compiler_test_case =
  { input: string
  ; expected_constants: Obj.item list
  ; expected_instructions: byte list list }

let test_instructions expected actual =
  let concatted = List.concat expected in
  Alcotest.(check int)
    "instruction length" (List.length expected) (List.length actual) ;
  Alcotest.(check (list char)) "each bytecode" concatted actual

let test_constants expected actual =
  Alcotest.(check int)
    "Constant list length" (List.length expected) (List.length actual) ;
  let helper exp act =
    match (exp, act) with
    | Obj.Int e, Obj.Int a ->
        Alcotest.(check int) "Int Constant check" e a
    | _ ->
        Alcotest.(fail) "Not yet implemented"
  in
  List.iter2 helper expected actual

let parse input = Lex.new' input |> Parsing.new_parser |> Parsing.parse_program

let run_compiler_tests tests =
  let helper {input; expected_constants; expected_instructions} =
    let program = parse input in
    let compiled = Compiler.compile program 10 in
    let bytecode = Compiler.bytecode_compiler compiled in
    test_instructions expected_instructions bytecode.instructions_byte ;
    test_constants expected_constants bytecode.constants_byte
  in
  List.iter helper tests

let test_int_arithmetic () =
  let tests =
    [ { input= "1 + 2"
      ; expected_constants= [Obj.Int 1; Obj.Int 2]
      ; expected_instructions= [make OpConstant [0]; make OpConstant [1]] } ]
  in
  run_compiler_tests tests

let () =
  Alcotest.run "OpConstant arithmetic checking"
    [ ( "testing compiler"
      , [Alcotest.test_case "int arithmetic" `Quick test_int_arithmetic] ) ]
