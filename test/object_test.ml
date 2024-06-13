open Object

let test_string_hash () =
  let hello1, hello2 = (Obj.String "Hello World", Obj.String "Hello World") in
  let diff1, diff2 =
    (Obj.String "My name is johnny", Obj.String "My name is johnny")
  in
  Alcotest.(check string)
    "Hash1"
    (Obj.hash_key hello1 |> Obj.item_to_string)
    (Obj.hash_key hello2 |> Obj.item_to_string) ;
  Alcotest.(check string)
    "Hash2"
    (Obj.hash_key diff1 |> Obj.item_to_string)
    (Obj.hash_key diff2 |> Obj.item_to_string)

let () =
  Alcotest.run "Object tests"
    [ ( "Hashing"
      , [Alcotest.test_case "test object hashing" `Quick test_string_hash] ) ]
