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
      |> add "b" (n_symbol "b" GLOBAL 1)
      |> add "c" (n_symbol "c" LOCAL 0)
      |> add "d" (n_symbol "d" LOCAL 1)
      |> add "e" (n_symbol "e" LOCAL 0)
      |> add "f" (n_symbol "f" LOCAL 1) )
  in
  let global = new_symbol_table () in
  let global, a = define "a" global in
  Alcotest.(check alc_symbol)
    "checking symbols a"
    (StringMap.find "a" expected)
    a ;
  let global, b = define "b" global in
  Alcotest.(check alc_symbol)
    "checking symbols b"
    (StringMap.find "b" expected)
    b ;
  let first_local = new_enclosed_symbol_table global in
  let first_local, c = define "c" first_local in
  Alcotest.(check alc_symbol)
    "checking symbols c"
    (StringMap.find "c" expected)
    c ;
  let first_local, d = define "d" first_local in
  Alcotest.(check alc_symbol)
    "checking symbols d"
    (StringMap.find "d" expected)
    d ;
  let second_local = new_enclosed_symbol_table first_local in
  let second_local, e = define "e" second_local in
  Alcotest.(check alc_symbol)
    "checking symbols e"
    (StringMap.find "e" expected)
    e ;
  let _, f = define "f" second_local in
  Alcotest.(check alc_symbol)
    "checking symbols f"
    (StringMap.find "f" expected)
    f

let[@ocaml.warning "-26"] test_resolve_local () =
  let global = new_symbol_table () in
  let global, _ = define "a" global in
  let global, _ = define "b" global in
  let local = new_enclosed_symbol_table global in
  let local, _ = define "c" local in
  let local, _ = define "d" local in
  let expected =
    [ n_symbol "a" GLOBAL 0
    ; n_symbol "b" GLOBAL 1
    ; n_symbol "c" LOCAL 0
    ; n_symbol "d" LOCAL 1 ]
  in
  let helper expected =
    Alcotest.(check (result alc_symbol Code.CodeError.alcotest_error))
      "Checking resolve a and b" (Ok expected)
      (resolve expected.name local)
  in
  List.iter helper expected

let[@ocaml.warning "-26"] test_resolve_local_nested () =
  let global = new_symbol_table () in
  let global, _ = define "a" global in
  let global, _ = define "b" global in
  let local = new_enclosed_symbol_table global in
  let local, _ = define "c" local in
  let local, _ = define "d" local in
  let second_local = new_enclosed_symbol_table local in
  let second_local, _ = define "e" second_local in
  let second_local, _ = define "f" second_local in
  let expected =
    [ ( local
      , [ n_symbol "a" GLOBAL 0
        ; n_symbol "b" GLOBAL 1
        ; n_symbol "c" LOCAL 0
        ; n_symbol "d" LOCAL 1 ] )
    ; ( second_local
      , [ n_symbol "a" GLOBAL 0
        ; n_symbol "b" GLOBAL 1
        ; n_symbol "e" LOCAL 0
        ; n_symbol "f" LOCAL 1 ] ) ]
  in
  let helper (table, expected_symbols) =
    List.iter
      (fun symbol ->
        Alcotest.(check (result alc_symbol Code.CodeError.alcotest_error))
          "Checking resolve a and b" (Ok symbol)
          (resolve symbol.name table) )
      expected_symbols
  in
  List.iter helper expected

let[@ocaml.warning "-26"] test_define_resolve_builtin () =
  let expected =
    StringMap.(
      empty
      |> add "a" (n_symbol "a" BUILTIN 0)
      |> add "c" (n_symbol "c" BUILTIN 1)
      |> add "e" (n_symbol "e" BUILTIN 2)
      |> add "f" (n_symbol "f" BUILTIN 3) )
  in
  let global = new_symbol_table () in
  let first_local = new_enclosed_symbol_table global in
  let second_local = new_enclosed_symbol_table first_local in
  let _item =
    StringMap.fold
      (fun name _symbol _acc ->
        let _ = define name global in
        "" )
      expected ""
  in
  ()

let () =
  Alcotest.run "Symbol Table Tests"
    [ ("Symbol init", [Alcotest.test_case "define" `Quick test_define])
    ; ("resolve", [Alcotest.test_case "define" `Quick test_resolve_global])
    ; ( "reslove local"
      , [Alcotest.test_case "resolve local" `Quick test_resolve_local] )
    ; ( "reslove local nested"
      , [Alcotest.test_case "nested" `Quick test_resolve_local_nested] )
    ; ( "resolve and define builtins"
      , [Alcotest.test_case "builtin" `Quick test_define_resolve_builtin] ) ]
