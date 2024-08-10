open Program_stack

let[@ocaml.warning "-9"] clean_up_test array =
  let ip = Array.length array in
  {ip; stack= Array.map (fun a -> Some a) array}

(* let pop_tests () = *)
(*   let tests = *)
(*     [ ([|1; 2; 3|], 3, [|None; None; None|]) *)
(*     ; ([|4; 5; 7; 10; 5; 3|], 2, [|Some 4; Some 5; Some 7; Some 10; None; None|]) *)
(*     ] *)
(*     |> List.map (fun (arr, b, c) -> (array_to_option arr, b, c)) *)
(*   in *)
(*   let helper (starting_stack, n, expected) = *)
(*     let range = List.init n (fun _ -> None) in *)
(*     let actual = List.fold_left (fun a _ -> pop a) starting_stack range in *)
(*     Alcotest.(check (array (option int))) "pop check" expected actual *)
(*   in *)
(*   List.iter helper tests *)

let push_tests () =
  let tests =
    [([1; 2; 3], [|1; 2; 3|]); ([7; 40; 80; 460; 5], [|7; 40; 80; 460; 5|])]
    |> List.map (fun (inp, act) -> (inp, clean_up_test act))
  in
  let helper (input, expected) =
    let program_stack = make_stack (List.length input) in
    List.iter (fun item -> push item program_stack) input ;
    Alcotest.(check alc_program_stack) "array check" program_stack expected
  in
  List.iter helper tests

let () =
  Alcotest.run "Stack program tests"
    [ ( "Stack program"
      , [ Alcotest.test_case "push" `Quick push_tests
          (* ; Alcotest.test_case "pop" `Quick pop_tests *) ] ) ]
