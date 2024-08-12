open Symbol_table

let parse input = Lex.new' input |> Parsing.new_parser |> Parsing.parse_program

let ( let* ) = Result.bind

module StringMap = Map.Make (String)

let test_resolve_global () =
  let global = new_symbol_table () in
  let global, _ = define "a" global in
  let global, _ = define "b" global in
  let expected = [n_symbol "a" GLOBAL 0; n_symbol "b" GLOBAL 1] in
  let helper expected =
    Alcotest.(check (result alc_symbol Code.CodeError.alcotest_error))
      "Checking resolve a and b" (Ok expected)
      (resolve expected.name global)
  in
  List.iter helper expected

let[@ocaml.warning "-26"] test_define () =
  let expected =
    StringMap.(
      empty
      |> add "a" (n_symbol "a" GLOBAL 0)
      |> add "b" (n_symbol "b" GLOBAL 1) )
  in
  let global = new_symbol_table () in
  let global, a = define "a" global in
  Alcotest.(check alc_symbol)
    "checking symbols a"
    (StringMap.find "a" expected)
    a ;
  let _, b = define "b" global in
  Alcotest.(check alc_symbol)
    "checking symbols b"
    (StringMap.find "b" expected)
    b

let () =
  Alcotest.run "Symbol Table Tests"
    [ ("Symbol init", [Alcotest.test_case "define" `Quick test_define])
    ; ("resolve", [Alcotest.test_case "define" `Quick test_resolve_global]) ]
