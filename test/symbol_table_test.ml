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

(* let a = 1; *)
(* let b = 2; *)

(* let firstLocal = fn() { *)
(*   let c = 3; *)
(*   let d = 4; *)
(*   a + b + c + d; *)

(*   let secondLocal = fn() { *)
(*     let e = 5; *)
(*     let f = 6; *)
(*     a + b + c + d + e + f; *)
(*   }; *)
(* }; *)
(* above code is tested here *)
let test_resolve_free () =
  let global = new_symbol_table () in
  let global, _ = define "a" global in
  let global, _ = define "b" global in
  let first_local = new_enclosed_symbol_table global in
  let first_local, _ = define "c" first_local in
  let first_local, _ = define "d" first_local in
  let second_local = new_enclosed_symbol_table first_local in
  let second_local, _ = define "e" second_local in
  let second_local, _ = define "f" second_local in
  let expected =
    let n = n_symbol in
    [ ( first_local
      , [n "a" GLOBAL 0; n "b" GLOBAL 1; n "c" LOCAL 0; n "d" LOCAL 1]
      , [] )
    ; ( second_local
      , [ n "a" GLOBAL 0
        ; n "b" GLOBAL 1
        ; n "c" FREE 0
        ; n "d" FREE 1
        ; n "e" LOCAL 0
        ; n "f" LOCAL 1 ]
      , [n "c" FREE 0; n "d" FREE 1] ) ]
  in
  (* Alcotest.(check (result alc_symbol Code.CodeError.alcotest_error)) *)
  (*   "Checking resolve a and b" (Ok symbol) *)
  (*   (resolve symbol.name table) ) *)
  let helper (table, expected_symbols, expected_free_symbols) =
    let iter_symbols symbol =
      let result = resolve symbol.name table in
      Alcotest.(check (result alc_symbol Code.CodeError.alcotest_error))
        "checking resolve" (Ok symbol) result
    in
    let iter_free_symbols index free_symbol =
      match table.free_symbols with
      (* the scope needs to have symbols *)
      | [] ->
          ()
      | _ ->
          let result = Ok (List.nth table.free_symbols index) in
          Alcotest.(check (result alc_symbol Code.CodeError.alcotest_error))
            "checking resolve" (Ok free_symbol) result
    in
    List.iter iter_symbols expected_symbols ;
    List.iteri iter_free_symbols expected_free_symbols
  in
  List.iter helper expected

let test_unresolved_free () =
  let global = new_symbol_table () in
  let global, _ = define "a" global in
  let first_local = new_enclosed_symbol_table global in
  let first_local, _ = define "c" first_local in
  let second_local = new_enclosed_symbol_table first_local in
  let second_local, _ = define "e" second_local in
  let second_local, _ = define "f" second_local in
  let expected =
    let n = n_symbol in
    [n "a" GLOBAL 0; n "c" FREE 0; n "e" LOCAL 0; n "f" LOCAL 1]
  in
  (* Alcotest.(check (result alc_symbol Code.CodeError.alcotest_error)) *)
  (*   "Checking resolve a and b" (Ok symbol) *)
  (*   (resolve symbol.name table) ) *)
  let helper expected_symbol =
    let result = resolve expected_symbol.name second_local in
    Alcotest.(check (result alc_symbol Code.CodeError.alcotest_error))
      "checking resolve" (Ok expected_symbol) result
  in
  let helper_unresolve name =
    let result = resolve name second_local in
    Alcotest.(check (result alc_symbol Code.CodeError.alcotest_error))
      "checking resolve"
      (Error (Code.CodeError.SymbolNotFound ("GLOBAL", name)))
      result
  in
  let expected_unresolve = ["b"; "d"] in
  List.iter helper_unresolve expected_unresolve ;
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
      , [Alcotest.test_case "builtin" `Quick test_define_resolve_builtin] )
    ; ( "resolve and define free"
      , [ Alcotest.test_case "free" `Quick test_resolve_free
        ; Alcotest.test_case "free" `Quick test_unresolved_free ] ) ]
