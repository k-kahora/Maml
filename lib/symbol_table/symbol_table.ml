let ( let* ) = Result.bind

type symbol_scope = GLOBAL | LOCAL | BUILTIN | FREE | FUNCTION

let scope_to_string = function
  | GLOBAL ->
      "GLOBAL"
  | LOCAL ->
      "LOCAL"
  | BUILTIN ->
      "BUILTIN"
  | FREE ->
      "FREE"
  | FUNCTION ->
      "FUNCTION"

type symbol = {name: string; scope: symbol_scope; index: int}

let symbol_eq s1 s2 = s1 = s2

let string_of_symbol symb =
  Format.sprintf "{name=%s; scope=%s; index=%d}" symb.name
    (scope_to_string symb.scope)
    symb.index

let symbol_pp fmt symb = Format.fprintf fmt "%s" (string_of_symbol symb)

let alc_symbol = Alcotest.testable symbol_pp symbol_eq

let n_symbol name scope index = {name; scope; index}

module StringMap = Map.Make (String)

let string_of_store store =
  StringMap.fold
    (fun key value acc ->
      acc ^ Format.sprintf "%s: %s; " key (string_of_symbol value) )
    store ""

type symbol_table =
  { mutable store: symbol StringMap.t
  ; num_definitions: int
  ; outer: symbol_table option
  ; mutable free_symbols: symbol list }

let define_function_name name st =
  let symbol = {name; index= 0; scope= FUNCTION} in
  let store = StringMap.add name symbol st.store in
  st.store <- store ;
  (st, symbol)

let define_builtin index name st =
  let symbol = {name; index; scope= BUILTIN} in
  let store = StringMap.add name symbol st.store in
  (symbol, {st with store})

let free_symbols_string lst =
  List.fold_left (fun acc next -> acc ^ string_of_symbol next) "" lst

let rec symbol_table_string symb_tb =
  Format.sprintf "{store=[%s];\nnum_definitions=%d;\nfree=[%s];\nouter=\n%s\n}"
    (string_of_store symb_tb.store)
    symb_tb.num_definitions
    (free_symbols_string symb_tb.free_symbols)
    (Option.fold ~none:"None" ~some:symbol_table_string symb_tb.outer)

let symbol_table_pp fmt symb_tb =
  Format.fprintf fmt "%s" (symbol_table_string symb_tb)

let symbol_table_eq st1 st2 = StringMap.equal ( = ) st1.store st2.store

let alc_symbol_table = Alcotest.testable symbol_table_pp symbol_table_eq

let both_eq (st1, symb1) (st2, symb2) =
  symbol_table_eq st1 st2 && symbol_eq symb1 symb2

let both_pp fmt (st, symb) =
  Format.fprintf fmt "symbol -> %s\ntable -> %s" (string_of_symbol symb)
    (symbol_table_string st)

let alc_symbol_table_and_symbol = Alcotest.testable both_pp both_eq

let new_symbol_table () =
  {store= StringMap.empty; num_definitions= 0; outer= None; free_symbols= []}

let new_enclosed_symbol_table symbol_table =
  let st = new_symbol_table () in
  {st with outer= Some symbol_table}

let define name st =
  let scope = Option.fold ~none:GLOBAL ~some:(fun _ -> LOCAL) st.outer in
  let symbol = n_symbol name scope st.num_definitions in
  let store = StringMap.add name symbol st.store in
  ({st with num_definitions= st.num_definitions + 1; store}, symbol)

(** [define_free original st] takes the originally local variable and defines a new free variable withint the context of the current scope *)
let define_free original st =
  let new_free_symbols = st.free_symbols @ [original] in
  let symbol =
    n_symbol original.name FREE @@ (List.length new_free_symbols - 1)
  in
  let new_store = StringMap.add original.name symbol st.store in
  st.store <- new_store ;
  st.free_symbols <- new_free_symbols ;
  symbol

(* NOTE this originally used a Option.fold with recursive calls and unded up having unsighly errors *)
let resolve name st =
  (* let _ = symbol_table_string st |> print_endline in *)
  let error = Code.CodeError.SymbolNotFound ("Global symbol not found", name) in
  let rec resolve_helper symbol_table =
    let* current_symbol_table = Option.to_result ~none:error symbol_table in
    (* The current scope so this would be local *)
    match StringMap.find_opt name current_symbol_table.store with
    | None -> (
      match resolve_helper current_symbol_table.outer with
      | Ok (_, symbol) ->
          if symbol.scope = GLOBAL || symbol.scope = BUILTIN then Ok (st, symbol)
          else Ok (st, define_free symbol current_symbol_table)
      | Error _ as err ->
          err )
    | Some a ->
        Ok (st, a)
  in
  resolve_helper (Some st)
