let dummy_token =
  let open Token in
  {type'= EOF; literal= "fail"}

let set_up_program input =
  Lexer.new' input |> Parser.new_parser |> Parser.parse_program

let test_bool_object expected = function
  | Object.Bool actual ->
      Alcotest.(check bool) "Checking bool object" expected actual
  | _ ->
      failwith "needs to be an bool object"

let test_null_object value =
  Alcotest.(check string) "null check" "Null" (Object.item_to_string value)

let test_int_object expected = function
  | Object.Int actual ->
      Alcotest.(check int) "Checking int object" expected actual
  | _ ->
      failwith "needs to be an int object"

let test_eval input =
  Lexer.new' input |> Parser.new_parser |> Parser.parse_program
  |> Evaluater.eval

let test_eval_bool_exp () =
  let tests =
    [ ("true", true)
    ; ("false", false)
    ; ("1 < 2", true)
    ; ("1 > 2", false)
    ; ("1 < 1", false)
    ; ("1 > 1", false)
    ; ("10000 > 9999", true)
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
    ; ("(1 > 2) == false", true) ]
  in
  List.iter
    (fun (input, expected) ->
      let evaluated = test_eval input in
      test_bool_object expected evaluated )
    tests

let test_if_else_expression () =
  let tests =
    [ ("if (true) { 10 }", Some 10)
    ; ("if (false) { 10 }", None)
    ; ("if (1) { 10 }", Some 10)
    ; ("if (1 < 2) { 10 }", Some 10)
    ; ("if (1 > 2) { 10 }", None)
    ; ("if (1 > 2) { 10 } else { 20 }", Some 20)
    ; ("if (1 < 2) { 10 } else { 20 }", Some 10) ]
  in
  List.iter
    (fun (input, expected) ->
      let evaluated = test_eval input in
      match expected with
      | Some value ->
          test_int_object value evaluated
      | None ->
          test_null_object evaluated )
    tests

let test_bang_operator () =
  let tests =
    [ ("!true", false)
    ; ("!false", true)
    ; ("!5", false)
    ; ("!!true", true)
    ; ("!!false", false)
    ; ("!!5", true) ]
  in
  List.iter
    (fun (input, expected) ->
      let evaluated = test_eval input in
      test_bool_object expected evaluated )
    tests

let test_eval_integer_exp () =
  let tests =
    [ ("5", 5)
    ; ("10", 10)
    ; ("-5", -5)
    ; ("-10", -10)
    ; ("5 + 5 + 5 + 5 - 10", 10)
    ; ("2 * 2 * 2 * 2 * 2", 32)
    ; ("-50 + 100 + -50", 0)
    ; ("5 * 2 + 10", 20)
    ; ("5 + 2 * 10", 25)
    ; ("20 + 2 * -10", 0)
    ; ("50 / 2 * 2 + 10", 60)
    ; ("2 * (5 + 10)", 30)
    ; ("3 * 3 * 3 + 10", 37)
    ; ("3 * (3 * 3) + 10", 37)
    ; ("2 * (20 - 100) + 160", 0)
    ; ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50) ]
  in
  List.iter
    (fun (input, expected) ->
      let evaluated = test_eval input in
      test_int_object expected evaluated )
    tests

let () =
  let open Alcotest in
  let _tests = [(("5", 5), ("10", 10))] in
  run "eval"
    [ ( "evaluation int expressions"
      , [ test_case "Testing evaluatin integer literals" `Quick
            test_eval_integer_exp ] )
    ; ( "evaluation boolean expressions"
      , [test_case "Testing evaluatin bool literals" `Quick test_eval_bool_exp]
      )
    ; ("testing bang operator", [test_case "bang" `Quick test_bang_operator])
    ; ( "testing the if expression"
      , [ test_case "testing if expression evalaution" `Quick
            test_if_else_expression ] ) ]
