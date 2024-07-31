open Code

let test_iter = List.iter

let test_make () =
  let tests =
    [ (OpConstant 65534, ['\x01'; '\xFF'; '\xFE'])
    ; (OpConstant 2, ['\x01'; '\x00'; '\x02']) ]
  in
  test_iter
    (fun (opcode, expected) ->
      Alcotest.(check (list char)) "Checking opcode make" (make opcode) expected
      )
    tests

let[@ocaml.warning "-26-27"] test_read_operands () =
  let tests = [(OpConstant 65535, 2)] in
  let helper (operands, bytes_read) =
    let instruction = make operands in
    let operands_read = read_operands operands instruction in
    ()
  in
  test_iter helper tests

let test_instruction_string () =
  let instructions =
    [make @@ OpConstant 1; make @@ OpConstant 2; make @@ OpConstant 65534]
    |> List.concat
  in
  let expected =
    {|
0000 OpConstant 1
0003 OpConstant 2
0006 OpConstant 65535
    |}
  in
  Alcotest.(check string)
    "byte list string" expected
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
