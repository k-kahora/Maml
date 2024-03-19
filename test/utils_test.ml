let blank () = ()

type test_type = ONE | TWO | THREE | FOUR

let test_assoc_list () =
  let open Utils in
  let w = Token_AssocList.(empty |> add ONE "hello" |> add TWO "bye bye") in
  let _x = Token_AssocList.find ONE w in
  let _y = Token_AssocList.find TWO w in
  print_endline (Option.get _y) ;
  match _x with None -> failwith "epic fail" | Some _x -> ()

let () =
  let open Alcotest in
  run "utils tests"
    [("Assoc list", [test_case "Test the asooc list" `Quick test_assoc_list])]
