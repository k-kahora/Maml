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
    compile program.statements compiler
    (* FIXME figure out why I need a bytecode DS *)
    (* let bytecode = bytecode compiler in *)
    (* bytecode *)
  in
  let helper (input, expected_constants, expected_instructions) =
    let concatted = List.concat expected_instructions in
    let expected_compiler =
      Ok
        { Compiler.new_compiler with
          instructions= concatted
        ; constants= expected_constants }
    in
    let actual = craft_compiler input in
    print_endline "expected" ;
    (* let _ = *)
    (*   Code.string_of_byte_list concatted *)
    (*   |> Result.fold ~error:CodeError.print_error ~ok:print_endline *)
    (* in *)
    (* print_endline "actual" ; *)
    (* let _ = *)
    (*   Code.string_of_byte_list (Result.get_ok actual |> fun a -> a.instructions) *)
    (*   |> Result.fold ~error:CodeError.print_error ~ok:print_endline *)
    (* in *)
    (* FIXME currently do not check constants and index *)
    Alcotest.(check (result alcotest_compiler Code.CodeError.alcotest_error))
      "Checking compiler" expected_compiler actual
  in
  List.iter helper tests

let map_test_helper obj_list =
  IntMap.of_list (List.mapi (fun idx obj -> (idx, obj)) obj_list)

let make_test_helper opcode_list =
  List.map (fun opcode -> make opcode) opcode_list

let test_int_arithmetic () =
  let open Object in
  let tests =
    [ ( "1 + 2" (* FIXME To much room for humean error in this test*)
      , map_test_helper [Obj.Int 1; Int 2]
      , make_test_helper [`Constant 0; `Constant 1; `Add; `Pop] )
    ; ( "1 + 2 + 3"
      , map_test_helper [Obj.Int 1; Int 2; Int 3]
      , make_test_helper
          [`Constant 0; `Constant 1; `Add; `Constant 2; `Add; `Pop] )
    ; ( "1; 2; 3"
      , map_test_helper [Obj.Int 1; Int 2; Int 3]
      , make_test_helper
          [`Constant 0; `Pop; `Constant 1; `Pop; `Constant 2; `Pop] )
    ; ( "1 - 4"
      , map_test_helper [Obj.Int 1; Int 4]
      , make_test_helper [`Constant 0; `Constant 1; `Sub; `Pop] )
    ; ( "1 * 4"
      , map_test_helper [Obj.Int 1; Int 4]
      , make_test_helper [`Constant 0; `Constant 1; `Mul; `Pop] )
    ; ( "2 / 1"
      , map_test_helper [Obj.Int 2; Int 1]
      , make_test_helper [`Constant 0; `Constant 1; `Div; `Pop] )
    ; ( "-1"
      , map_test_helper [Obj.Int 1]
      , make_test_helper [`Constant 0; `Minus; `Pop] ) ]
  in
  run_compiler_tests tests

let test_bool_expressions () =
  let open Object.Obj in
  let tests =
    [ ("false", map_test_helper [], make_test_helper [`False; `Pop])
    ; ("true", map_test_helper [], make_test_helper [`True; `Pop])
    ; ( "1 == 2"
      , map_test_helper [Int 1; Int 2]
      , make_test_helper [`Constant 0; `Constant 1; `Equal; `Pop] )
    ; ( "1 > 2"
      , map_test_helper [Int 1; Int 2]
      , make_test_helper [`Constant 0; `Constant 1; `GreaterThan; `Pop] )
    ; ( "1 < 2"
      , map_test_helper [Int 2; Int 1]
      , make_test_helper [`Constant 0; `Constant 1; `GreaterThan; `Pop] )
    ; ( "true == false"
      , map_test_helper []
      , make_test_helper [`True; `False; `Equal; `Pop] )
    ; ( "1 != 2"
      , map_test_helper [Int 1; Int 2]
      , make_test_helper [`Constant 0; `Constant 1; `NotEqual; `Pop] )
    ; ( "true != false"
      , map_test_helper []
      , make_test_helper [`True; `False; `NotEqual; `Pop] )
    ; ("!false", map_test_helper [], make_test_helper [`False; `Bang; `Pop]) ]
  in
  run_compiler_tests tests

let test_hash_expressions () =
  let open Object.Obj in
  let tests =
    [ ("{}", map_test_helper [], make_test_helper [`Hash 0; `Pop])
    ; ( "{1: 2, 3: 4, 5: 6}"
      , map_test_helper [Int 1; Int 2; Int 3; Int 4; Int 5; Int 6]
      , make_test_helper
          [ `Constant 0
          ; `Constant 1
          ; `Constant 2
          ; `Constant 3
          ; `Constant 4
          ; `Constant 5
          ; `Hash 6
          ; `Pop ] )
    ; ( "{1: 2 + 3, 4: 5 * 6}"
      , map_test_helper [Int 1; Int 2; Int 3; Int 4; Int 5; Int 6]
      , make_test_helper
          [ `Constant 0
          ; `Constant 1
          ; `Constant 2
          ; `Add
          ; `Constant 3
          ; `Constant 4
          ; `Constant 5
          ; `Mul
          ; `Hash 4
          ; `Pop ] ) ]
  in
  run_compiler_tests tests

let test_global_let_statement () =
  let open Object in
  let tests =
    [ ( {|let one = 1; let two = 2|}
      , map_test_helper [Obj.Int 1; Int 2]
      , make_test_helper [`Constant 0; `SetGlobal 0; `Constant 1; `SetGlobal 1]
      )
    ; ( "let one = 1; one"
      , map_test_helper [Obj.Int 1]
      , make_test_helper [`Constant 0; `SetGlobal 0; `GetGlobal 0; `Pop] )
    ; ( {|let one = 1; one;|}
      , map_test_helper [Obj.Int 1]
      , make_test_helper [`Constant 0; `SetGlobal 0; `GetGlobal 0; `Pop] )
    ; ( {| let one = 1; let two = one; two; |}
      , map_test_helper [Obj.Int 1]
      , make_test_helper
          [ `Constant 0
          ; `SetGlobal 0
          ; `GetGlobal 0
          ; `SetGlobal 1
          ; `GetGlobal 1
          ; `Pop ] ) ]
  in
  run_compiler_tests tests

let test_conditionals () =
  let open Object in
  let tests =
    [ ( "if (true) { 10 }; 3333;"
      , map_test_helper [Obj.Int 10; Int 3333]
      , make_test_helper
          [ `True
          ; `JumpNotTruthy 10
          ; `Constant 0
          ; `Jump 11
          ; `Null
          ; `Pop
          ; `Constant 1
          ; `Pop ] )
    ; ( "if (true) { 10 } else { 20 }; 3333;"
      , map_test_helper [Obj.Int 10; Int 20; Int 3333]
      , make_test_helper
          [ `True
          ; `JumpNotTruthy 10
          ; `Constant 0
          ; `Jump 13
          ; `Constant 1
          ; `Pop
          ; `Constant 2
          ; `Pop ] )
    ; ( "if (true) { 10 }; 3333;"
      , map_test_helper [Obj.Int 10; Int 3333]
      , make_test_helper
          [ `True
          ; `JumpNotTruthy 10
          ; `Constant 0
          ; `Jump 11
          ; `Null
          ; `Pop
          ; `Constant 1
          ; `Pop ] ) ]
  in
  run_compiler_tests tests

let test_string_expression () =
  let open Object in
  let tests =
    [ ( {|"monkey"|}
      , map_test_helper [Obj.String "monkey"]
      , make_test_helper [`Constant 0; `Pop] )
    ; ( {|"mon" + "key"|}
      , map_test_helper [Obj.String "mon"; String "key"]
      , make_test_helper [`Constant 0; `Constant 1; `Add; `Pop] ) ]
  in
  run_compiler_tests tests

let test_array_expression () =
  let open Object in
  let tests =
    [ ("[]", map_test_helper [], make_test_helper [`Array 0; `Pop])
    ; ( "[1,2,3]"
      , map_test_helper [Obj.Int 1; Int 2; Int 3]
      , make_test_helper [`Constant 0; `Constant 1; `Constant 2; `Array 3; `Pop]
      )
    ; ( "[1 + 2, 3 - 4, 5 * 6]"
      , map_test_helper [Obj.Int 1; Int 2; Int 3; Int 4; Int 5; Int 6]
      , make_test_helper
          [ `Constant 0
          ; `Constant 1
          ; `Add
          ; `Constant 2
          ; `Constant 3
          ; `Sub
          ; `Constant 4
          ; `Constant 5
          ; `Mul
          ; `Array 3
          ; `Pop ] ) ]
  in
  run_compiler_tests tests

let () =
  Alcotest.run "OpConstant arithmetic checking"
    [ ( "testing compiler"
      , [ Alcotest.test_case "int arithmetic" `Quick test_int_arithmetic
        ; Alcotest.test_case "bool expressions" `Quick test_bool_expressions
        ; Alcotest.test_case "conditionals" `Quick test_conditionals ] )
    ; ( "let bindings"
      , [Alcotest.test_case "bindings" `Quick test_global_let_statement] )
    ; ( "string compilation"
      , [Alcotest.test_case "string work" `Quick test_string_expression] )
    ; ( "array comp"
      , [Alcotest.test_case "array work" `Quick test_array_expression] )
    ; ( "hash compilation"
      , [Alcotest.test_case "hash work" `Quick test_hash_expressions] ) ]
