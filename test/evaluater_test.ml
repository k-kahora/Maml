open Object

(* This is exclusivley used for the builtin test function *)
type int_string = Int of int | String of string | Array of int array

let dummy_token =
  let open Token in
  {type'= EOF; literal= "fail"}

let set_up_program input =
  Lexer.new' input |> Parser.new_parser |> Parser.parse_program

let test_bool_object expected = function
  | Obj.Bool actual ->
      Alcotest.(check bool) "Checking bool object" expected actual
  | _ ->
      failwith "needs to be an bool object"

let test_null_object value =
  Alcotest.(check string) "null check" "Null" (Obj.item_to_string value)

let test_error_object expected = function
  | Obj.Error actual ->
      Alcotest.(check string) "Checking error object" expected actual
  | a ->
      failwith ("needs to be an error object got " ^ Obj.item_to_string a)

let test_int_array_object expected = function
  | Obj.Array actual ->
      Alcotest.(check (array int))
        "Checking int array" expected
        (Array.map
           (fun a ->
             match a with
             | Obj.Int i ->
                 i
             | a ->
                 failwith
                   ( Format.sprintf "needs to be an array of ints got %s"
                   @@ Obj.item_to_string a ) )
           actual )
  | a ->
      failwith ("needs to be an int array object got" ^ Obj.item_to_string a)

let test_int_object expected = function
  | Obj.Int actual ->
      Alcotest.(check int) "Checking int object" expected actual
  | a ->
      failwith ("needs to be an int object got" ^ Obj.item_to_string a)

let test_eval (input : string) : Obj.item =
  Lexer.new' input |> Parser.new_parser |> Parser.parse_program
  |> Evaluater.eval (Environment.new_environment ())

let test_eval_bool_exp () =
  let tests =
    [ ("true", true)
    ; ("false", false)
    ; ("1 < 2", true)
    ; ("1 > 2", false)
    ; ("1 < 1", false)
    ; ("1 > 1", false)
    ; ("1 >= 1", true)
    ; ("1 <= 1", true)
    ; ("2 <= 1", false)
    ; ("1 >= 2", false)
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

let test_return_statement () =
  let tests =
    [ ("return 10;", 10)
    ; ("return 9; 10;", 9)
    ; ("return 2 * 6; 9;", 12)
    ; ("if (true) {return 13;} return 3", 13)
    ; ("if (10 > 1) {\nif (10 > 1) {\nreturn 9;\n}\n        return 1;\n}", 9)
    ; ("9; return 2 * 5; 9;", 10) ]
  in
  List.iter
    (fun (input, expected) ->
      let evaluated = test_eval input in
      test_int_object expected evaluated )
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

let test_error_handling () =
  let tests =
    [ ("5 + true;", "type mismatch: INTEGER + BOOLEAN")
    ; ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN")
    ; ("-true", "unknown operator: -BOOLEAN")
    ; ("true + false;", "unknown operator: BOOLEAN + BOOLEAN")
    ; ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN")
    ; ("if (10 > 1) { true + false; }", "unknown operator: BOOLEAN + BOOLEAN")
    ; ("foobar", "identifier not found: foobar")
    ; ("\"Hello\" - \"World\"", "unknown operator: STRING - STRING")
    ; ( "if (10 > 1) {\n\
        \  if (10 > 1) {\n\
        \    return true + false;\n\
        \  }\n\n\
        \  return 1;"
      , "unknown operator: BOOLEAN + BOOLEAN" ) ]
    (* ; ("foobar", "identifier not found: foobar") ] *)
  in
  List.iter
    (fun (input, expected) ->
      let evaluated = test_eval input in
      test_error_object expected evaluated )
    tests

let test_function_application () =
  let tests =
    [ ("let identity = fn(x) { x; }; identity(5);", 5)
    ; ("let identity = fn(x) { return x; }; identity(5);", 5)
    ; ("let double = fn(x) { x * 2; }; double(5);", 10)
    ; ("let add = fn(x, y) { x + y; }; add(5, 5);", 10)
    ; ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20)
    ; ("fn(x) { x; }(5)", 5) ]
  in
  List.iter
    (fun (input, expected) -> test_int_object expected (test_eval input))
    tests

let test_map () =
  let tests =
    [ ( {|

let map = fn(arr, f) {
  let iter = fn(arr, accumulated) {
    if (len(arr) == 0) {
      accumulated
    } else {
      iter(rest(arr), push(accumulated, f(first(arr))));
    }
  };

  iter(arr, []);
};map([1,2,3],fn(x) {x * 4})|}
      , [|2; 4; 6|] ) ]
  in
  List.iter
    (fun (input, expected) ->
      let evaluated = test_eval input in
      test_int_array_object expected evaluated )
    tests
(*   let tests = *)

let test_recursive () =
  let tests =
    [ ( "let fib = fn(n) {if (n <= 1) {return n} else {return fib(n - 1) + \
         fib(n - 2)}}; fib(9)"
      , 34 ) ]
    (*   let tests = *)

    (* [ ( {|let fib = fn(n) { *)
       (*              if (n <= 1) { *)
       (*                 return n; *)
       (*              } else { *)
       (*              return fib(n - 1) + fib(n - 2);} *)
       (*           }; fib(4)  *)
       (* |} *)
    (*       , 13 ) ] *)
  in
  List.iter
    (fun (input, expected) ->
      let evaluated = test_eval input in
      test_int_object expected evaluated )
    tests

let test_let_statements () =
  let tests =
    [ ("let a = 5; a", 5)
    ; ("let a = 5 * 5; a;", 25)
    ; ("let z = 5 * 5; let x = -5; z + x;", 20)
    ; ("let a = 5; let b = a; b;", 5)
    ; ("let a = 5; let b = a; let c = a + b + 5; c;", 15) ]
  in
  List.iter
    (fun (input, expected) ->
      let evaluated = test_eval input in
      test_int_object expected evaluated )
    tests

let test_function_object () =
  let input = "fn(x) { x + 2 };" in
  let parameters, body, _env =
    match test_eval input with
    | Obj.Function (p, b, e) ->
        (p, b, e)
    | _ ->
        failwith "object is not a function"
  in
  let p =
    match List.hd parameters with
    | Ast.Identifier i ->
        i.value
    | _ ->
        failwith "parameter need to be a ident"
  in
  let body = Ast.statement_str body in
  Alcotest.(check int) "Parameter list length" 1 (List.length parameters) ;
  Alcotest.(check string) "Parameter name" "x" p ;
  Alcotest.(check string) "Cheking body" "(x + 2)" body

let test_string_literal () =
  let input = {|"Hello World"|} in
  let evaluated = test_eval input in
  match evaluated with
  | Obj.String str ->
      Alcotest.(check string) "Checking object string" "Hello World" str
  | _ ->
      failwith "not an object string"

let test_string_concat () =
  let input = "\"Hello\" + \" \" +  \"World\"" in
  let evaluated = test_eval input in
  match evaluated with
  | Obj.String str ->
      Alcotest.(check string) "String concat test" "Hello World" str
  | _ ->
      failwith "Object is not a string"

let test_closures () =
  let input =
    {|let newAdder = fn(x,y) {
        fn(z) {x + y - z};
      };
      let addTwo = newAdder(2,4);
      addTwo(10);|}
  in
  test_int_object (-4) @@ test_eval input

let test_builtin_length () =
  let tests =
    [ ({|len(" ")|}, Some (Int 1))
    ; ({|len(" four ")|}, Some (Int 6))
    ; ({|len("hello world")|}, Some (Int 11))
    ; ({|len(1)|}, Some (String "argument to `len` not supported, got INTEGER"))
    ; ( {|len(" one ", " two ")|}
      , Some (String "wrong number of arguments. got=2, want=1") )
    ; ({|len([1, 2, 3])|}, Some (Int 3))
    ; ({|len([])|}, Some (Int 0))
      (* ; ({|puts(" hello ", " world !")|}, None) *)
    ; ({|first([1, 2, 3])|}, Some (Int 1))
    ; ({|first([])|}, None)
    ; ( {|first(1)|}
      , Some (String "argument to `first` must be ARRAY, got INTEGER") )
    ; ({|last([1, 2, 3])|}, Some (Int 3))
    ; ({|last([1, 2, 3])|}, Some (Int 3))
    ; ({|last([1, 2, 3, 4, 5, 6, 7])|}, Some (Int 7))
    ; ({|last([])|}, None)
    ; ( {|last(1)|}
      , Some (String "argument to `last` must be ARRAY, got INTEGER") )
    ; ({|rest([1, 2, 3])|}, Some (Array [|2; 3|]))
    ; ({|rest([1, 2, 3, 4, 5, 6])|}, Some (Array [|2; 3; 4; 5; 6|]))
    ; ({|rest([])|}, None)
    ; ({|push([], 1)|}, Some (Array [|1|]))
    ; ({|push([1,2,3], 1)|}, Some (Array [|1; 2; 3; 1|]))
    ; ( {|push(1, 1)|}
      , Some (String "argument to `push` must be ARRAY, got INTEGER") ) ]
  in
  List.iter
    (fun (input, expected) ->
      let evaluated = test_eval input in
      match expected with
      | Some e -> (
        match e with
        | String str -> (
          match evaluated with
          | Obj.Error e ->
              Alcotest.(check string) "string literal" str e
          | _ ->
              failwith "should be an error" )
        | Int i ->
            test_int_object i evaluated
        | Array arr ->
            test_int_array_object arr evaluated )
      | None ->
          test_null_object evaluated )
    tests

let test_array_index_expressions () =
  let tests =
    [ ("[1, 2, 3][0]", Some 1)
    ; ("[1, 2, 3][1]", Some 2)
    ; ("[1, 2, 3][2]", Some 3)
    ; ("let i = 0; [1][i];", Some 1)
    ; ("[1, 2, 3][1 + 1];", Some 3)
    ; ("let myArray = [1, 2, 3]; myArray[2];", Some 3)
    ; ("let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", Some 6)
    ; ("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", Some 2)
    ; ("[1, 2, 3][3]", None)
    ; ("[1, 2, 3][-1]", None) ]
  in
  List.iter
    (fun (input, expected) ->
      let evaluated = test_eval input in
      match expected with
      | Some value ->
          print_endline (Obj.item_to_string evaluated) ;
          test_int_object value evaluated
      | None ->
          test_null_object evaluated )
    tests

let test_array_literals () =
  let input = "[1, 2 * 2, 3 + 3]" in
  let evaluated = test_eval input in
  match evaluated with
  | Obj.Array arr ->
      Alcotest.(check int) "Array length check" 3 (Array.length arr) ;
      test_int_object 1 arr.(0) ;
      test_int_object 4 arr.(1) ;
      test_int_object 6 arr.(2)
  | _ ->
      failwith "Object is not a string"

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
            test_if_else_expression ] )
    ; ( "testing return statements"
      , [ test_case "testing return expression evalaution" `Quick
            test_return_statement ] )
    ; ( "testing errors"
      , [test_case "testing error logging" `Quick test_error_handling] )
    ; ( "testing functions"
      , [test_case "low level func test" `Quick test_function_object] )
    ; ("testing clojures", [test_case "clojures" `Quick test_closures])
    ; ( "testing string literals"
      , [test_case "strings" `Quick test_string_literal] )
    ; ("testing string concat", [test_case "concat" `Quick test_string_concat])
    ; ("recursive test", [test_case "fibanci seq" `Quick test_recursive])
    ; ("mapping", [test_case "mapping function" `Quick test_map])
    ; ( "testing builtin functions"
      , [test_case "length check" `Quick test_builtin_length] )
    ; ( "testing array literals"
      , [ test_case "testing array literals index by index" `Quick
            test_array_literals ] )
    ; ( "testing array indexing"
      , [test_case "array indexing tests" `Quick test_array_index_expressions]
      )
    ; ( "testing function application"
      , [test_case "func app test" `Quick test_function_application] )
    ; ( "testing let bindings"
      , [test_case "binding test" `Quick test_let_statements] ) ]
