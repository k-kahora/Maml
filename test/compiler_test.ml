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
    let compiler = new_compiler () in
    compile program.statements compiler
    (* FIXME figure out why I need a bytecode DS *)
    (* let bytecode = bytecode compiler in *)
    (* bytecode *)
  in
  let helper (input, expected_constants, expected_instructions) =
    let concatted = List.concat expected_instructions in
    let main_scope =
      { last_instruction= {opcode= `Null; position= 0}
      ; previous_instruction= {opcode= `Null; position= 0}
      ; instructions= concatted }
    in
    let expected_compiler =
      Ok
        { (Compiler.new_compiler ()) with
          scopes= [|main_scope|]
        ; constants= expected_constants }
    in
    let actual = craft_compiler input in
    (* let _ = *)
    (*   Code.string_of_byte_list concatted *)
    (*   |> Result.fold ~error:CodeError.print_error ~ok:print_endline *)
    (* in *)
    (* print_endline "actual" ; *)
    (* let _ = *)
    (*   Code.string_of_byte_list *)
    (*     (Result.get_ok actual |> fun a -> Compiler.current_instructions a) *)
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

let test_index_expression () =
  let open Object in
  let tests =
    [ ( "[1, 2, 3][1 + 1]"
      , map_test_helper [Obj.Int 1; Int 2; Int 3; Int 1; Int 1]
      , make_test_helper
          [ `Constant 0
          ; `Constant 1
          ; `Constant 2
          ; `Array 3
          ; `Constant 3
          ; `Constant 4
          ; `Add
          ; `Index
          ; `Pop ] )
    ; ( "{1: 2}[2 - 1]"
      , map_test_helper [Obj.Int 1; Int 2; Int 2; Int 1]
      , make_test_helper
          [ `Constant 0
          ; `Constant 1
          ; `Hash 2
          ; `Constant 2
          ; `Constant 3
          ; `Sub
          ; `Index
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

let test_function_expressions () =
  let open Object.Obj in
  let tests =
    [ ( "fn () {return 10 * 2}"
      , map_test_helper
          [ Int 10
          ; Int 2
          ; CompFunc
              ( [ make (`Constant 0)
                ; make (`Constant 1)
                ; make `Mul
                ; make `ReturnValue ]
              |> List.concat ) ]
      , make_test_helper [`Constant 2; `Pop] )
    ; ( "fn () { 10 * 2}"
      , map_test_helper
          [ Int 10
          ; Int 2
          ; CompFunc
              ( [ make (`Constant 0)
                ; make (`Constant 1)
                ; make `Mul
                ; make `ReturnValue ]
              |> List.concat ) ]
      , make_test_helper [`Constant 2; `Pop] )
    ; ( "fn () {1; 2}"
      , map_test_helper
          [ Int 1
          ; Int 2
          ; CompFunc
              ( [ make (`Constant 0)
                ; make `Pop
                ; make (`Constant 1)
                ; make `ReturnValue ]
              |> List.concat ) ]
      , make_test_helper [`Constant 2; `Pop] )
    ; ( "fn () {}"
      , map_test_helper [CompFunc ([make `Return] |> List.concat)]
      , make_test_helper [`Constant 0; `Pop] ) ]
  in
  run_compiler_tests tests

let test_function_call () =
  let open Object.Obj in
  let tests =
    [ ( "fn() {25}()"
      , map_test_helper
          [ Int 25
          ; CompFunc ([make (`Constant 0); make `ReturnValue] |> List.concat) ]
      , make_test_helper [`Constant 1; `Call; `Pop] )
    ; ( "let no_arg = fn() { 25 }; no_arg();"
      , map_test_helper
          [ Int 25
          ; CompFunc ([make (`Constant 0); make `ReturnValue] |> List.concat) ]
      , make_test_helper [`Constant 1; `SetGlobal 0; `GetGlobal 0; `Call; `Pop]
      ) ]
  in
  run_compiler_tests tests

let test_let_stmt_scopes () =
  let open Object.Obj in
  let tests =
    [ ( {|let num = 55;
         fn() { num }|}
      , map_test_helper
          [ Int 55
          ; CompFunc ([make (`GetGlobal 0); make `ReturnValue] |> List.concat)
          ]
      , make_test_helper [`Constant 0; `SetGlobal 0; `Constant 1; `Pop] )
    ; ( {|fn() {
           let num = 55;
           num
         }
   |}
      , map_test_helper
          [ Int 55
          ; CompFunc
              ( [ make (`Constant 0)
                ; make (`SetLocal 0)
                ; make (`GetLocal 0)
                ; make `ReturnValue ]
              |> List.concat ) ]
      , make_test_helper [`Constant 1; `Pop] )
    ; ( {|
         fn() {
           let a = 55;
           let b = 77;
           a + b
         }
         |}
      , map_test_helper
          [ Int 55
          ; Int 77
          ; CompFunc
              ( [ make (`Constant 0)
                ; make (`SetLocal 0)
                ; make (`Constant 1)
                ; make (`SetLocal 1)
                ; make (`GetLocal 0)
                ; make (`GetLocal 1)
                ; make `Add
                ; make `ReturnValue ]
              |> List.concat ) ]
      , make_test_helper [`Constant 2; `Pop] ) ]
  in
  run_compiler_tests tests

let test_compiler_scopes () =
  let index_check expected cmp =
    Alcotest.(check int) "checking initial scope value" expected cmp.scope_index
  in
  let length_check expected cmp =
    Alcotest.(check int)
      "instruction length check" expected
      (List.length (current_instructions cmp))
  in
  let instruction_check marker expected cmp =
    match marker with
    | `Last ->
        Alcotest.(check string)
          "last opcode check"
          (Code.operand_name expected)
          (Code.operand_name_not_marker
             cmp.scopes.(cmp.scope_index).last_instruction.opcode )
    | `Previous ->
        Alcotest.(check string)
          "previous opcode check"
          (Code.operand_name expected)
          (Code.operand_name_not_marker
             cmp.scopes.(cmp.scope_index).previous_instruction.opcode )
  in
  let emit_h opcode cmp = emit opcode cmp |> fst in
  let cmp = new_compiler () in
  index_check 0 cmp ;
  let global_symbol_table = cmp.symbol_table in
  let cmp = emit_h `Mul cmp |> enter_scope in
  index_check 1 cmp ;
  let cmp = emit_h `Sub cmp in
  length_check 1 cmp ;
  instruction_check `Last `Sub cmp ;
  Alcotest.(check (option Symbol_table.alc_symbol_table))
    "outer symbol table should be the global symbal table"
    cmp.symbol_table.outer (Some global_symbol_table) ;
  let cmp = leave_scope cmp |> fst in
  Alcotest.(check (option Symbol_table.alc_symbol_table))
    "symbol table check after leave scope shold be global"
    (Some cmp.symbol_table) (Some global_symbol_table) ;
  Alcotest.(check (option Symbol_table.alc_symbol_table))
    "outer scope should be None" None cmp.symbol_table.outer ;
  index_check 0 cmp ;
  let cmp = emit_h `Add cmp in
  length_check 2 cmp ;
  instruction_check `Last `Add cmp ;
  instruction_check `Previous `Mul cmp

let () =
  Alcotest.run "OpConstant arithmetic checking"
    [ ( "testing compiler"
      , [ Alcotest.test_case "int arithmetic" `Quick test_int_arithmetic
        ; Alcotest.test_case "bool expressions" `Slow test_bool_expressions
        ; Alcotest.test_case "conditionals" `Slow test_conditionals ] )
    ; ( "let bindings"
      , [Alcotest.test_case "bindings" `Slow test_global_let_statement] )
    ; ( "string compilation"
      , [Alcotest.test_case "string work" `Slow test_string_expression] )
    ; ( "array comp"
      , [Alcotest.test_case "array work" `Slow test_array_expression] )
    ; ( "hash compilation"
      , [Alcotest.test_case "hash work" `Slow test_hash_expressions] )
    ; ( "index compilation"
      , [Alcotest.test_case "index work" `Slow test_index_expression] )
    ; ( "scope indexing"
      , [Alcotest.test_case "scope index chekinge" `Slow test_compiler_scopes]
      )
    ; ( "function compilation"
      , [Alcotest.test_case "function work" `Slow test_function_expressions] )
    ; ( "function calls"
      , [Alcotest.test_case "function calls work" `Slow test_function_call] )
    ; ( "stmt scopes"
      , [Alcotest.test_case "local scopes" `Slow test_let_stmt_scopes] ) ]
