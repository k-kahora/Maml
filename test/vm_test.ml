open Object

let parse input = Lex.new' input |> Parsing.new_parser |> Parsing.parse_program

let ( let* ) = Result.bind

let test_expected_object actual =
  match actual with
  | Obj.Int value2 ->
      Ok value2
  | obj ->
      Error (Code.CodeError.ObjectNotImplemented obj)

(* FIXME incorrect use of let* *)
let setup_vm_test input =
  let program = parse input in
  let comp = Compiler.new_compiler in
  let* comp = Compiler.compile program.statements comp in
  let vm = Vm.new_virtual_machine comp in
  let* res = Vm.run vm in
  let* stack_elem =
    Stack.top_opt res.stack |> Option.to_result ~none:Code.CodeError.EmptyStack
  in
  test_expected_object stack_elem

let run_vm_tests (input, expected) =
  let result = setup_vm_test input in
  Alcotest.(check (result int Code.CodeError.alcotest_error))
    "Checking result " expected result

let test_int_arithmatic () =
  let tests =
    [("1", 1); ("2", 2); ("1 + 2", 3); ("267 + 58 + 2", 327)]
    |> List.map (fun (a, b) -> (a, Ok b))
  in
  List.iter run_vm_tests tests

let () =
  Alcotest.run "Virtual Machine Tests"
    [ ( "Arithmatic"
      , [Alcotest.test_case "int arithmetic" `Quick test_int_arithmatic] ) ]
