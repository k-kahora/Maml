let ( let* ) = Result.bind

type symbol_scope = GLOBAL

let scope_to_string = function GLOBAL -> "GLOBAL"

type symbol = {name: string; scope: symbol_scope; index: int}

let symbol_eq s1 s2 = s1 = s2

let symbol_pp fmt symb =
  Format.fprintf fmt "{name=%s; scope=%s; index=%d}" symb.name
    (scope_to_string symb.scope)
    symb.index

let alc_symbol = Alcotest.testable symbol_pp symbol_eq

let n_symbol name scope index = {name; scope; index}

module StringMap = Map.Make (String)

type symbol_table = {store: symbol StringMap.t; num_definitions: int}

let new_symbol_table () = {store= StringMap.empty; num_definitions= 0}

let define name st =
  let symbol = n_symbol name GLOBAL st.num_definitions in
  let store = StringMap.add name symbol st.store in
  ({num_definitions= st.num_definitions + 1; store}, symbol)

let resolve name st =
  StringMap.find_opt name st.store
  |> Option.to_result ~none:(Code.CodeError.SymbolNotFound ("resolve", name))
