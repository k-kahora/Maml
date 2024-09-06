let ( let* ) = Result.bind

type symbol_scope = GLOBAL | LOCAL | BUILTIN | FREE

let scope_to_string = function
  | GLOBAL ->
      "GLOBAL"
  | LOCAL ->
      "LOCAL"
  | BUILTIN ->
      "BUILTIN"
  | FREE ->
      "FREE"

type symbol = {name: string; scope: symbol_scope; index: int}

let symbol_eq s1 s2 = s1 = s2

let string_of_symbol symb =
  Format.sprintf "{name=%s; scope=%s; index=%d}" symb.name
    (scope_to_string symb.scope)
    symb.index

let string_of_store _symb = Format.sprintf "store string not yet implemented"

let symbol_pp fmt symb = Format.fprintf fmt "%s" (string_of_symbol symb)

let alc_symbol = Alcotest.testable symbol_pp symbol_eq

let n_symbol name scope index = {name; scope; index}

module StringMap = Map.Make (String)

type symbol_table =
  { store: symbol StringMap.t
  ; num_definitions: int
  ; outer: symbol_table option
  ; free_symbols: symbol list }

let define_builtin index name st =
  let symbol = {name; index; scope= BUILTIN} in
  let store = StringMap.add name symbol st.store in
  (symbol, {st with store})

let rec symbol_table_string symb_tb =
  Format.sprintf "{store=%s; num_definitions=%d; outer=%s}"
    (string_of_store symb_tb.store)
    symb_tb.num_definitions
    (Option.fold ~none:"None" ~some:symbol_table_string symb_tb.outer)

let symbol_table_pp fmt symb_tb =
  Format.fprintf fmt "%s" (symbol_table_string symb_tb)

let symbol_table_eq st1 st2 = StringMap.equal ( = ) st1.store st2.store

let alc_symbol_table = Alcotest.testable symbol_table_pp symbol_table_eq

let new_symbol_table () =
  {store= StringMap.empty; num_definitions= 0; outer= None; free_symbols= []}

let new_enclosed_symbol_table symbol_table =
  { store= StringMap.empty
  ; num_definitions= 0
  ; outer= Some symbol_table
  ; free_symbols= symbol_table.free_symbols }

let define name st =
  let scope = Option.fold ~none:GLOBAL ~some:(fun _ -> LOCAL) st.outer in
  let symbol = n_symbol name scope st.num_definitions in
  let store = StringMap.add name symbol st.store in
  let symbol =
    ({st with num_definitions= st.num_definitions + 1; store}, symbol)
  in
  symbol

let define_free original st =
  let new_free_symbols = st.free_symbols @ [original] in
  let symbol =
    n_symbol original.name FREE @@ (List.length new_free_symbols - 1)
  in
  let new_store = StringMap.add original.name symbol st.store in
  ({st with store= new_store; free_symbols= new_free_symbols}, symbol)

let rec resolve name st =
  let error = Code.CodeError.SymbolNotFound ("Global symbol not found", name) in
  let obj = StringMap.find_opt name st.store in
  Option.fold
    ~some:(fun obj -> Ok obj)
    ~none:
      (let* outer_scope = st.outer |> Option.to_result ~none:error in
       let* (* _updated_st,  *) found_symbol = resolve name outer_scope in
       Ok found_symbol )
    obj

(* let rec resolve name st = *)
(*   let error = Code.CodeError.SymbolNotFound ("Global symbol not found", name) in *)
(*   let obj = StringMap.find_opt name st.store in *)
(*   Option.fold *)
(*     ~some:(fun obj -> Ok (st, obj)) *)
(*     ~none: *)
(*       (let* outer_scope = st.outer |> Option.to_result ~none:error in *)
(*        let* updated_st, found_symbol = resolve name outer_scope in *)
(*        Ok (updated_st, found_symbol) ) *)
(*     obj *)
