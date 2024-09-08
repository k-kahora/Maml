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
  { store: symbol StringMap.t
  ; num_definitions: int
  ; outer: symbol_table option
  ; free_symbols: symbol list }

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

let define_free original st =
  let new_free_symbols = st.free_symbols @ [original] in
  let symbol =
    n_symbol original.name FREE @@ (List.length new_free_symbols - 1)
  in
  let new_store = StringMap.add original.name symbol st.store in
  ({st with store= new_store; free_symbols= new_free_symbols}, symbol)

(* Hard to follow wrote it out for myself *)
(* Check if the symbol is in the local scope *)
(* If not check that the outerscope is not null if it ever is error because
   that would mean it was not in previous scope and theres no more scopes to look *)
(* If "ere is an outerscope simply resolve with this outer scope recursively" *)

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

let resolve name st =
  let error = Code.CodeError.SymbolNotFound ("Global symbol not found", name) in
  let rec resolve_helper current_scope =
    (* First if the outerscope is Null return an error *)
    let* outerscope = Option.to_result ~none:error current_scope.outer in
    let obj = StringMap.find_opt name outerscope.store in
    Option.fold
      ~some:(fun a ->
        if a.scope = GLOBAL || a.scope == BUILTIN then Ok (st, a)
        else Ok (define_free a st) )
      ~none:(resolve_helper outerscope)
      obj
  in
  (* First we find if it is in the current scope if not we try to resolve it by moving up the scope *)
  StringMap.find_opt name st.store
  |> Option.fold ~none:(resolve_helper st) ~some:(fun a -> Ok (st, a))
(* |> Option.to_result *)
(*      ~none:(Code.CodeError.SymbolNotFound ("Global symbol not found", name)) *)

(* let rec resolve name st = *)
(*   let error = Code.CodeError.SymbolNotFound ("Global symbol not found", name) in *)
(*   let obj = *)
(*     StringMap.find_opt name st.store *)
(*     |> Option.fold *)
(*          ~none: *)
(*            (let* outer_scope = Option.to_result ~none:error st.outer in *)
(*             let* new_st, new_obj = resolve name outer_scope in *)
(*             if new_obj.scope = GLOBAL || new_obj.scope = BUILTIN then *)
(*               Ok (new_st, new_obj) *)
(*             else define_free new_obj new_st |> Result.ok ) *)
(*          ~some:(fun a -> Ok (st, a)) *)
(*   in *)
(*   obj *)
