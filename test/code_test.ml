open Code

let test_make () =
  let tests = [(OpConstant, [65534], [Byte.byte OpConstant; '\xFF'; '\xFE'])] in
  List.iter
    (fun (opcode, parameter, expected) ->
      let byte_list = make opcode parameter in
      Alcotest.(check (list char)) "Checking byte list" expected byte_list )
    tests
(* print_bytes_seq (int_of_hex 65534) ; *)
(* assert (int_of_hex 65534 = []) *)

(* let test_read_operands () = *)
(*   let tests = [(OpConstant, 65534, 2)] in *)
(*   let helper (op, operands, bytesRead) = *)
(*     let instruction = make op operands in *)
(*     let def = lookup (List.hd instruction) in *)
(*     let read_operand = read_operands def instruction in *)
(*     Alcotest.(check int) "Bytes read" () *)
(*   in *)
(*   List.iter helper tests *)

let test_read_operands () =
  let tests = [(OpConstant, [65535], 2)] in
  let helper (opcode, operands, bytesRead) =
    let instruction = make opcode operands in
    let def = lookups opcode in
    let n, operand_read = read_operands def @@ List.tl instruction in
    assert (n = bytesRead) ;
    List.iter2
      (fun expected actual ->
        Alcotest.(check int) "checking operands" expected actual )
      operands operand_read
  in
  List.iter helper tests

let test_instruction_string () =
  let instructions =
    [make OpConstant [1]; make OpConstant [2]; make OpConstant [65535]]
  in
  let expected =
    {|0000 OpConstant 1
 0003 OpConstant 2
 0006 OpConstant 65535
|}
  in
  let concatted = List.concat instructions in
  let actual =
    List.fold_left (fun acc nxt -> acc ^ Byte.string_of_byte nxt) "" concatted
  in
  Alcotest.(check string) "Opcode strings" expected actual

let () =
  Alcotest.run "OpCode Testing"
    [ ( "testing make and read_operands"
      , [ Alcotest.test_case "testing make operand code" `Quick test_make
        ; Alcotest.test_case "testing read operand code" `Quick
            test_read_operands ] )
    ; ( "testing strings conversion"
      , [ Alcotest.test_case "opconstant string check" `Quick
            test_instruction_string ] ) ]
