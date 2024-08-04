open Compiler
module IntMap = Map.Make (Int)

let ( let* ) = Result.bind

let parse input = Lex.new' input |> Parsing.new_parser |> Parsing.parse_program

open Code

let test_iter = List.iter

let test_instructions expected actual =
  let expected = List.concat expected in
  Alcotest.(check int)
    "Instruction lengths" (List.length expected) (List.length actual) ;
  Alcotest.(check (list char)) "Actual instruction values" expected actual

let test_constants _ _ = ()

let[@ocaml.warning "-27"] run_compiler_tests tests =
  let craft_compiler input =
    let program = parse input in
    let compiler = new_compiler in
    let* compile_result = compile compiler program.statements in
    Ok compile_result
    (* FIXME figure out why I need a bytecode DS *)
    (* let bytecode = bytecode compiler in *)
    (* bytecode *)
  in
  let helper (input, expected_constants, expected_instructions) =
    let concatted = List.concat expected_instructions in
    let expected_compiler =
      Ok {instructions= concatted; index= 0; constants= expected_constants}
    in
    let actual = craft_compiler input in
    (* FIXME currently do not check constants and index *)
    Alcotest.(check (result alcotest_compiler Code.CodeError.alcotest_error))
      "Checking compiler" expected_compiler actual
  in
  List.iter helper tests

let test_int_arithmetic () =
  let open Object in
  let tests =
    [ ( "1 + 2" (* FIXME To much room for humean error in this test*)
      , IntMap.(empty |> add 0 (Obj.Int 1) |> add 1 (Obj.Int 2))
      , [make @@ `OpConstant 0; make @@ `OpConstant 1] ) ]
  in
  run_compiler_tests tests

let () =
  Alcotest.run "OpConstant arithmetic checking"
    [ ( "testing compiler"
      , [Alcotest.test_case "int arithmetic" `Quick test_int_arithmetic] ) ]
