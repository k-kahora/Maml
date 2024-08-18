open Object

let parse input = Lex.new' input |> Parsing.new_parser |> Parsing.parse_program

let ( let* ) = Result.bind

type vm_test_type =
  | Int of int
  | Bool of bool
  | String of string
  | Array of Obj.item array
  | Hash of (Obj.item, Obj.hash_item) Hashtbl.t

let compare_hash_tables h1 h2 =
  let f h = Hashtbl.to_seq_keys h |> List.of_seq in
  let h1_keys, h2_keys = (f h1, f h2) in
  let h1_values, h2_values = (f h1, f h2) in
  h1_keys = h2_keys && h1_values = h2_values

let compare_test_type t1 t2 =
  match (t1, t2) with
  | Some (Hash h1), Some (Hash h2) ->
      compare_hash_tables h1 h2
  | _ ->
      t1 = t2

let pp_test_type fmt test_type =
  let format_helper = Format.fprintf in
  Option.fold ~none:(format_helper fmt "")
    ~some:(function
      | Int a ->
          format_helper fmt "%d" a
      | Bool a ->
          format_helper fmt "%b" a
      | String a ->
          format_helper fmt "%s" a
      | Array arr -> (
        match arr with
        | [||] ->
            format_helper fmt "[]"
        | _ ->
            let str =
              Array.fold_left
                (fun acc element ->
                  acc
                  ^ Format.sprintf "%s, " (Object.Obj.item_to_string element) )
                "[ " arr
            in
            let str = String.sub str 0 (String.length str - 2) ^ "]" in
            format_helper fmt "%s" str )
      | Hash hash ->
          let str_helper _hash_code {Obj.value; key} str =
            (* Format.printf "hash_code: %s" (Obj.item_to_string hash_code) ; *)
            let value = Obj.item_to_string value in
            let key = Obj.item_to_string key in
            str ^ key ^ ": " ^ value ^ ", "
          in
          let str = Hashtbl.fold str_helper hash "{" in
          format_helper fmt "%s" @@ str ^ "}" )
    test_type

let alcotest_test_type = Alcotest.testable pp_test_type compare_test_type

let test_expected_object actual =
  match actual with
  | Obj.Int value ->
      Ok (Some (Int value))
  | Obj.Bool value ->
      Ok (Some (Bool value))
  | Obj.String value ->
      Ok (Some (String value))
  | Obj.Array value ->
      Ok (Some (Array value))
  | Obj.Null ->
      Ok None
  | Obj.Hash value ->
      Ok (Some (Hash value))
  | obj ->
      Error (Code.CodeError.ObjectNotImplemented obj)

(* FIXME incorrect use of let* *)
let setup_vm_test input =
  let program = parse input in
  let comp = Compiler.new_compiler in
  let* comp = Compiler.compile program.statements comp in
  Code.ByteFmt.pp_byte_list comp.instructions |> print_endline ;
  let _ =
    Code.string_of_byte_list comp.instructions
    |> Result.fold ~error:Code.CodeError.print_error ~ok:print_endline
  in
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
    ; ("!!5", true)
    ; ("!(if (false) {5;})", true) ]
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
    ; ("if ( 1 > 2) { 10 }", None)
    ; ("if ( 1 > 2) { 10 }", None)
    ; ("if ((if (false) { 10 })) { 10 } else { 20 }", Some 20) ]
    |> List.map (fun (a, b) -> (a, Ok (option_mapper b)))
  in
  List.iter run_vm_tests tests

let test_global_let_stmt () =
  let tests =
    [ ("let one = 1; one", 1)
    ; ("let one = 1; let two = 2; one + two", 3)
    ; ("let one = 1; let two = one + one; one + two", 3) ]
    |> List.map (fun (a, b) -> (a, Ok (Some (Int b))))
  in
  List.iter run_vm_tests tests

let test_string_expressions () =
  let tests =
    [ ({|"monkey"|}, "monkey")
    ; ({|"monkey"|}, "monkey")
    ; ({| "mon" + "key" |}, "monkey")
    ; ({| "mon" + "key" + "banana" |}, "monkeybanana") ]
    |> List.map (fun (a, b) -> (a, Ok (Some (String b))))
  in
  List.iter run_vm_tests tests

let test_array_literals () =
  let tests =
    [ ("[]", [||])
    ; ("[1,2,3]", [|Obj.Int 1; Int 2; Int 3|])
    ; ("[1 + 2, 3 * 4, 5 + 6]", [|Obj.Int 3; Int 12; Int 11|]) ]
    |> List.map (fun (a, b) -> (a, Ok (Some (Array b))))
  in
  List.iter run_vm_tests tests

let make_hash lst =
  let helper hash (key, value) =
    let hash_item = {Obj.key; value} in
    Hashtbl.add hash (Obj.hash_key key) hash_item ;
    hash
  in
  List.fold_left helper (Hashtbl.create 65535) lst

let test_hash_literals () =
  let tests =
    [ ("{}", make_hash [])
    ; ( {|{1: 2, 2: 3, "ape": false  }|}
      , make_hash [(Int 1, Int 2); (Int 2, Int 3); (String "ape", Bool false)]
      )
    ; ( "{1 + 1: 2 * 2, 3 + 3: 4 * 4}"
      , make_hash [(Int 2, Int 4); (Int 6, Int 16)] ) ]
    |> List.map (fun (a, b) -> (a, Ok (Some (Hash b))))
  in
  List.iter run_vm_tests tests

let test_hash_literals () =
  let option_mapper (a, b) = (a, Ok (Option.map (fun d -> Int d) b)) in
  let tests =
    [ ("[1, 2, 3][1]", Some 2)
    ; ("[1, 2, 3][0 + 2]", Some 3)
    ; ("[[1, 1, 1]][0][0]", Some 1)
    ; ("[][0]", None)
    ; ("[1, 2, 3][99]", None)
    ; ("[1][-1]", None)
    ; ("{1: 1, 2: 2}[1]", Some 1)
    ; ("{1: 1, 2: 2}[2]", Some 2)
    ; ("{1: 1}[0]", None)
    ; ("{}[0]", None) ]
    |> List.map option_mapper
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
      , [Alcotest.test_case "test conditionals" `Quick test_conditionals] )
    ; ( "global let statements"
      , [Alcotest.test_case "let statements" `Quick test_global_let_stmt] )
    ; ( "strings"
      , [Alcotest.test_case "string monkey tests" `Quick test_string_expressions]
      )
    ; ( "arrays"
      , [Alcotest.test_case "array monkey tests" `Quick test_array_literals] )
    ; ( "hash tables"
      , [Alcotest.test_case "hash table tests" `Quick test_hash_literals] ) ]
