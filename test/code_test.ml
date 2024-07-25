open Code

let test_operand_code () =
  let tests = [(OpConstant, 65534, ['\x01'; '\xFF'; '\xFE'])] in
  List.iter
    (fun (opcode, parameter, expected) ->
      let byte_list = make opcode parameter in
      Alcotest.(check (list char))
        "OpConstant byte conversion" expected byte_list )
    tests
(* print_bytes_seq (int_of_hex 65534) ; *)
(* assert (int_of_hex 65534 = []) *)

let () =
  Alcotest.run "OpCode Testing"
    [ ( "testing make"
      , [Alcotest.test_case "testing make operand code" `Quick test_operand_code]
      ) ]
