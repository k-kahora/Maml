open Code

let test_iter = List.iter

let printf = Format.printf

let hexidecimal_tests () =
  let tests =
    [(['\x17'; '\xEB'; '\x92'], 2, 6123); (['\x17'; '\xEB'; '\x92'], 3, 1567634)]
  in
  List.iter
    (fun (input, length, expected) ->
      Alcotest.(check int)
        "Checking int_of_hex" expected
        (ByteFmt.int_of_hex input length) )
    tests

let test_make () =
  let tests =
    [ (`Constant 65534, ['\x01'; '\xFF'; '\xFE'])
    ; (`Constant 2, ['\x01'; '\x00'; '\x02']) ]
  in
  test_iter
    (fun (opcode, expected) ->
      Alcotest.(check (list char)) "Checking opcode make" (make opcode) expected
      )
    tests

let[@ocaml.warning "-26-27"] test_read_operands () =
  let tests = [(`Constant 65535, 2)] in
  let helper (operands, bytes_read) =
    let instruction = make operands in
    let operands_read, bytes_read = read_operands `OPCONSTANT instruction in
    ()
  in
  test_iter helper tests

let test_instruction_string () =
  let instructions =
    [make @@ `Add; make @@ `Constant 2; make @@ `Constant 65535] |> List.concat
  in
  let expected = {|
0000 Add
0001 Constant 2
0004 Constant 65535|} in
  Alcotest.(check (result string Code.CodeError.alcotest_error))
    "byte list string" (Ok expected)
    (string_of_byte_list instructions)

let () =
  Alcotest.run "OpCode Testing"
    [ ( "testing make and read_operands"
      , [ Alcotest.test_case "testing make operand code" `Quick test_make
        ; Alcotest.test_case "testing read operand code" `Quick
            test_read_operands ] )
    ; ( "testing strings conversion"
      , [ Alcotest.test_case "opconstant string check" `Quick
            test_instruction_string ] ) ]

let x = ['\x01'; '\x00'; '\x03'; '\x01'; '\x00'; '\x03'; '\x01'; '\xFF'; '\xFE']

let () =
  Alcotest.run "libray testsing"
    [ ( "Testing Hexidecimal conversions"
      , [Alcotest.test_case "int_of_hex" `Quick hexidecimal_tests] ) ]
