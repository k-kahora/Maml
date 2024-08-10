open Object

let parse input = Lex.new' input |> Parsing.new_parser |> Parsing.parse_program

let ( let* ) = Result.bind

type vm_test_type = Int of int | Bool of bool

let compare_test_type t1 t2 = t1 = t2

let pp_test_type fmt test_type =
  let format_helper = Format.fprintf in
  Option.fold ~none:(format_helper fmt "None")
    ~some:(function
      | Int a -> format_helper fmt "%d" a | Bool a -> format_helper fmt "%b" a
      )
    test_type

let alcotest_test_type = Alcotest.testable pp_test_type compare_test_type

let test_expected_object actual =
  match actual with
  | Obj.Int value2 ->
      Ok (Some (Int value2))
  | Obj.Bool value2 ->
      Ok (Some (Bool value2))
  | Obj.Null ->
      Ok None
  | obj ->
      Error (Code.CodeError.ObjectNotImplemented obj)

(* FIXME incorrect use of let* *)
let setup_vm_test input =
  let program = parse input in
  let comp = Compiler.new_compiler in
  let* comp = Compiler.compile program.statements comp in
  (* let _ = *)
  (*   Code.string_of_byte_list comp.instructions *)
  (*   |> Result.fold ~error:Code.CodeError.print_error ~ok:print_endline *)
  (* in *)
  let vm = Vm.new_virtual_machine comp in
  let* res = Vm.run vm in
  let stack_elem = res.last_item_poped in
  test_expected_object stack_elem

let run_vm_tests (input, expected) =
  let result = setup_vm_test input in
  (* Result.fold ~error:Code.CodeError.print_error *)
  (*   ~ok:(fun _ -> print_endline "Got it") *)
  (*   result ; *)
  Alcotest.(check (result alcotest_test_type Code.CodeError.alcotest_error))
    "Checking result " expected result

let test_bool_expressions () =
  let tests =
    [ ("true", true)
    ; ("false", false)
    ; ("1 < 2", true)
    ; ("1 > 2", false)
    ; ("1 < 1", false)
    ; ("1 > 1", false)
    ; ("1 == 1", true)
    ; ("1 != 1", false)
    ; ("1 == 2", false)
    ; ("1 != 2", true)
    ; ("true == true", true)
    ; ("false == false", true)
    ; ("true == false", false)
    ; ("true != false", true)
    ; ("false != true", true)
    ; ("(1 < 2) == true", true)
    ; ("(1 < 2) == false", false)
    ; ("(1 > 2) == true", false)
    ; ("(1 > 2) == false", true)
    ; ("!true", false)
    ; ("!false", true)
    ; ("!5", false)
    ; ("!!true", true)
    ; ("!!false", false)
    ; ("!!5", true) ]
    |> List.map (fun (a, b) -> (a, Ok (Some (Bool b))))
  in
  List.iter run_vm_tests tests

let test_int_arithmatic () =
  let tests =
    [ ("1 - 2", -1)
    ; ("1 * 2", 2)
    ; ("4 / 2", 2)
    ; ("50 / 2 * 2 + 10 - 5", 55)
    ; ("5 + 5 + 5 + 5 - 10", 10)
    ; ("2 * 2 * 2 * 2 * 2", 32)
    ; ("5 * 2 + 10", 20)
    ; ("5 + 2 * 10", 25)
    ; ("5 * (2 + 10)", 60)
    ; ("-5", -5)
    ; ("-10", -10)
    ; ("-50 + 100 + -50", 0)
    ; ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50) ]
    |> List.map (fun (a, b) -> (a, Ok (Some (Int b))))
  in
  List.iter run_vm_tests tests

let test_conditionals () =
  let option_mapper opt = Option.map (fun a -> Int a) opt in
  let tests =
    [ ("if (true) { 10 }", Some 10)
    ; ("if (true) { 10 } else { 40 }", Some 10)
    ; ("if (false) { 10 } else { 20 } ", Some 20)
    ; ("if (1) { 10 }", Some 10)
    ; ("if (1 < 2) { 10 }", Some 10)
    ; ("if (1 < 2) { 10 } else { 20 }", Some 10)
    ; ("if (1 > 2) { 10 } else { 20 }", Some 20)
    ; ("if ( 1 > 2) { 10 }", None) (* ("if (false) { 10 }", None) *) ]
    |> List.map (fun (a, b) -> (a, Ok (option_mapper b)))
  in
  List.iter run_vm_tests tests

let () =
  Alcotest.run "Virtual Machine Tests"
    [ ( "Arithmatic"
      , [Alcotest.test_case "int arithmetic" `Quick test_int_arithmatic] )
    ; ( "Booleans"
      , [ Alcotest.test_case "boolean expressions vm" `Quick
            test_bool_expressions ] )
    ; ( "conditionals"
      , [Alcotest.test_case "test conditionals" `Quick test_conditionals] ) ]
