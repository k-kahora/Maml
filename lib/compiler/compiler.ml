open Object
module IntMap = Map.Make (Int)

let obj_map = IntMap.empty

type byte = char

module TestObj = struct
  let compare o1 o2 =
    let open Object.Obj in
    match (o1, o2) with
    | Int a, Int b ->
        Int.compare a b
    | String a, String b ->
        String.compare a b
    | CompFunc (lst1, _), CompFunc (lst2, _) ->
        List.compare (fun a b -> int_of_char a - int_of_char b) lst1 lst2
    | _ ->
        -1
end

let ( let* ) = Result.bind

type emitted_instruction = {opcode: Code.opcode; position: int}

type compilation_scope =
  { instructions: byte list
  ; last_instruction: emitted_instruction
  ; previous_instruction: emitted_instruction }

let null_scope =
  let null_emitted = {opcode= `Null; position= 0} in
  { instructions= []
  ; last_instruction= null_emitted
  ; previous_instruction= null_emitted }

type compiler =
  { index: int
  ; scope_index: int
  ; constants: Obj.item IntMap.t
  ; symbol_table: Symbol_table.symbol_table
  ; scopes: compilation_scope array }

let current_instructions cmp = cmp.scopes.(cmp.scope_index).instructions

let current_scope cmp = cmp.scopes.(cmp.scope_index)

let update_current_scope scope cmp = cmp.scopes.(cmp.scope_index) <- scope

(** [enter_scope cmp] when a function is compiled a new scope will be set as the current scope and a new symbol table shall be created to accomadet local bindings *)
let enter_scope cmp =
  { cmp with
    scope_index= cmp.scope_index + 1
  ; symbol_table= Symbol_table.new_enclosed_symbol_table cmp.symbol_table }

(** [leave_scope cmp] called once a function is finished compiling, symbol_table is then brought back to the enclosed oute scope *)
let leave_scope cmp =
  let cur_inst = current_instructions cmp in
  let symbol_table =
    cmp.symbol_table.outer |> Option.value ~default:cmp.symbol_table
  in
  cmp.scopes.(cmp.scope_index) <- null_scope ;
  ({cmp with scope_index= cmp.scope_index - 1; symbol_table}, cur_inst)

let set_last_instruction opcode position cmp =
  let cur_scope = current_scope cmp in
  let previous = cur_scope.last_instruction in
  let updated_scope =
    { cur_scope with
      previous_instruction= previous
    ; last_instruction= {opcode; position} }
  in
  update_current_scope updated_scope cmp

let pp_compiler fmt cmp =
  let pp_map map =
    IntMap.fold
      (fun key value acc ->
        let string =
          match value with
          (* | Obj.CompFunc (ls, _) -> *)
          (*     let item = *)
          (*       Code.string_of_byte_list ls *)
          (*       |> function *)
          (*       | Ok str -> str | Error err -> Code.CodeError.error_string err *)
          (*     in *)
          (*     item *)
          | a ->
              Obj.item_to_string a
        in
        acc ^ Format.sprintf "%d:%s, " key string )
      map "{"
  in
  (* let item = *)
  (*   Code.string_of_byte_list (current_instructions cmp) *)
  (*   |> function Ok str -> str | Error err -> Code.CodeError.error_string err *)
  (* in *)
  (* Format.fprintf fmt "Instructions: %s\nConstants: %s}" *)
  (* (Code.ByteFmt.pp_byte_list (current_instructions cmp)) *)
  (* item (pp_map cmp.constants) *)
  Format.fprintf fmt "Instructions: %s\nConstants: %s}"
    (Code.ByteFmt.pp_byte_list (current_instructions cmp))
    (pp_map cmp.constants)

let equal_compiler c1 c2 =
  let c1_c, c2_c = (current_instructions c1, current_instructions c2) in
  let instructions =
    List.compare (fun a b -> int_of_char a - int_of_char b) c1_c c2_c = 0
  in
  let constants =
    IntMap.compare TestObj.compare c1.constants c2.constants = 0
  in
  instructions && constants

let alcotest_compiler = Alcotest.testable pp_compiler equal_compiler

let new_compiler () =
  let empty_array = Array.init 65535 (fun _ -> null_scope) in
  { index= 0
  ; scope_index= 0
  ; constants= IntMap.empty
  ; symbol_table= Symbol_table.new_symbol_table ()
  ; scopes= empty_array }

let new_with_state symbol_table constants =
  let n_cmp = new_compiler () in
  {n_cmp with symbol_table; constants}

let empty_symbol_table_and_constants () =
  (Symbol_table.new_symbol_table (), IntMap.empty)

let rec add_constants obj cmp =
  ( { cmp with
      constants= IntMap.add cmp.index obj cmp.constants
    ; index= cmp.index + 1 }
  , cmp.index )

and emit op cmp =
  let inst = Code.make op in
  let pos = add_instructions inst cmp in
  set_last_instruction op pos cmp ;
  (cmp, pos)

and add_instructions inst cmp =
  let cur_inst = current_instructions cmp in
  let pos_new_inst = List.length cur_inst in
  let updated_instructions = cur_inst @ inst in
  let cur_scope = current_scope cmp in
  let new_scope = {cur_scope with instructions= updated_instructions} in
  update_current_scope new_scope cmp ;
  pos_new_inst

let replace_instruction pos new_instruction cmp =
  let f pos l1 l2 =
    let count = ref (-1) in
    let end_pos = List.length l1 + pos in
    List.mapi
      (fun idx item ->
        if idx >= pos && idx < end_pos then
          let _ = count := !count + 1 in
          List.nth l1 !count
        else item )
      l2
  in
  let patched_instructions = f pos new_instruction (current_instructions cmp) in
  let cur_scope = current_scope cmp in
  let new_scope = {cur_scope with instructions= patched_instructions} in
  update_current_scope new_scope cmp ;
  current_scope cmp

(**  [change_operand op_pos operand cmp] Assumes you only replace operands of the same type and same length *)
let[@ocaml.warning "-9"] change_operand op_pos operand cmp =
  let* {def} = List.nth (current_instructions cmp) op_pos |> Code.lookup in
  let* marker = Code.marker_to_opcode operand def in
  let new_instruction = Code.make marker in
  Ok (replace_instruction op_pos new_instruction cmp)

(** [remove_pop cmp] this needs to be called when compiling if expressions because when a if expression is compilled the consequence is considered a expressions and an additonal pop is pushed onto the stack*)
let remove_pop cmp =
  let[@ocaml.tailcall] rec pop_last = function
    | [] ->
        []
    | [_] ->
        []
    | hd :: tl ->
        hd :: pop_last tl
  in
  let cur_scope = current_scope cmp in
  let old = cur_scope.instructions in
  let new_inst = pop_last old in
  let new_scope =
    { cur_scope with
      instructions= new_inst
    ; last_instruction= cur_scope.previous_instruction }
  in
  update_current_scope new_scope cmp

let last_instruction_is opcode cmp =
  (* Defensive check *)
  if List.length (current_instructions cmp) = 0 then false
  else (current_scope cmp).last_instruction.opcode = opcode

let[@ocaml.warning "-27-9-26"] rec compile nodes cmp =
  let open Ast in
  let rec compile_expression expr cmp =
    match expr with
    (* NOTE IfExpressions need to be back patched *)
    (* FIXME Cleant this up needs serious work  *)
    | IfExpression {condition; consquence; altenative} ->
        (* NOTE Compile the condition *)
        let* cmp = compile_expression condition cmp in
        (* NOTE Emit the jump instruction with garbage *)
        let cmp, jump_not_truth_pos = emit (`JumpNotTruthy 9999) cmp in
        (* NOTE Compile the consequence every time*)
        let* cmp = compile_node consquence cmp in
        (* NOTE Remove the duplicated pop*)
        let cmp =
          if last_instruction_is `Pop cmp then (remove_pop cmp ; cmp) else cmp
        in
        (* NOTE Get the position after the consquence*)
        let cmp, jump_pos = emit (`Jump 9999) cmp in
        let after_consequence_pos = List.length (current_instructions cmp) in
        let* _ = change_operand jump_not_truth_pos after_consequence_pos cmp in
        let* cmp =
          if Option.is_none altenative then
            let cmp, _ = emit `Null cmp in
            Ok cmp
          else
            let* cmp = compile_node (Option.get altenative) cmp in
            let cmp =
              if last_instruction_is `Pop cmp then (remove_pop cmp ; cmp)
              else cmp
            in
            Ok cmp
        in
        let after_alternative = List.length (current_instructions cmp) in
        let* _ = change_operand jump_pos after_alternative cmp in
        Ok cmp
    | InfixExpression {left; right; operator} -> (
        let lr_compile = function
          | `Left2Right ->
              let* left_compiled_cmp = compile_expression left cmp in
              compile_expression right left_compiled_cmp
          | `Right2Left ->
              let* right_compiled_cmp = compile_expression right cmp in
              compile_expression left right_compiled_cmp
        in
        match operator with
        | "+" ->
            let* cmp = lr_compile `Left2Right in
            Ok (emit `Add cmp |> fst)
        | "-" ->
            let* cmp = lr_compile `Left2Right in
            Ok (emit `Sub cmp |> fst)
        | "*" ->
            let* cmp = lr_compile `Left2Right in
            Ok (emit `Mul cmp |> fst)
        | "/" ->
            let* cmp = lr_compile `Left2Right in
            Ok (emit `Div cmp |> fst)
        | "==" ->
            let* cmp = lr_compile `Left2Right in
            Ok (emit `Equal cmp |> fst)
        | "<" ->
            let* cmp = lr_compile `Right2Left in
            Ok (emit `GreaterThan cmp |> fst)
        | ">" ->
            let* cmp = lr_compile `Left2Right in
            Ok (emit `GreaterThan cmp |> fst)
        | "!=" ->
            let* cmp = lr_compile `Left2Right in
            Ok (emit `NotEqual cmp |> fst)
        | a ->
            Error (Code.CodeError.UnknownOperator a) )
    | PrefixExpression {operator; right} -> (
        let* right_compiled = compile_expression right cmp in
        match operator with
        | "!" ->
            Ok (emit `Bang right_compiled |> fst)
        | "-" ->
            Ok (emit `Minus right_compiled |> fst)
        | a ->
            Error (Code.CodeError.UnknownOperator a) )
    | IntegerLiteral {value} ->
        let integer = Obj.Int value in
        let cmp, index = add_constants integer cmp in
        (* Format.printf "\nprevious_byte_list %s\n" *)
        (*   (current_instructions cmp |> Code.ByteFmt.pp_byte_list) ; *)
        let cmp, inst_pos = emit (`Constant index) cmp in
        (* Format.printf "\ncurrent_byte_list %s\n" *)
        (*   (current_instructions cmp |> Code.ByteFmt.pp_byte_list) ; *)
        Ok cmp
    | BooleanExpression {value} ->
        let cmp, _ = if value then emit `True cmp else emit `False cmp in
        Ok cmp
    | Identifier {value} ->
        (* NOTE Compilet time error *)
        let* symbol = Symbol_table.resolve value cmp.symbol_table in
        let cmp, _ =
          match symbol.scope with
          | GLOBAL ->
              emit (`GetGlobal symbol.index) cmp
          | LOCAL ->
              emit (`GetLocal symbol.index) cmp
        in
        Ok cmp
    | StringLiteral {value} ->
        let string = Obj.String value in
        let cmp, index = add_constants string cmp in
        let cmp, _ = emit (`Constant index) cmp in
        Ok cmp
    | ArrayLiteral {elements} ->
        let* cmp =
          List.fold_left
            (fun acc nxt ->
              let* cmp = acc in
              compile_expression nxt cmp )
            (Ok cmp) elements
        in
        let cmp, _ = emit (`Array (List.length elements)) cmp in
        Ok cmp
    | HashLiteral {pairs} ->
        let list = Hashtbl.to_seq pairs |> List.of_seq in
        let list =
          List.sort
            (fun (a, _) (b, _) ->
              String.compare (Ast.expression_str a) (Ast.expression_str b) )
            list
        in
        let* cmp =
          List.fold_left
            (fun acc (key, value) ->
              let* cmp = acc in
              let* cmp = compile_expression key cmp in
              compile_expression value cmp )
            (Ok cmp) list
        in
        let hash_length_2 = Hashtbl.length pairs * 2 in
        let cmp, _ = emit (`Hash hash_length_2) cmp in
        Ok cmp
    | IndexExpression {left; index} ->
        let* cmp = compile_expression left cmp in
        let* cmp = compile_expression index cmp in
        let cmp = emit `Index cmp |> fst in
        Ok cmp
    | FunctionLiteral {parameters; body} ->
        let replace_last_pop_with_return cmp =
          let cur_scope = current_scope cmp in
          let cur_last_inst_updated =
            {cur_scope.last_instruction with opcode= `ReturnValue}
          in
          let last_pos = cur_scope.last_instruction.position in
          let new_scope =
            replace_instruction last_pos (Code.make `ReturnValue) cmp
          in
          update_current_scope
            {new_scope with last_instruction= cur_last_inst_updated}
            cmp
        in
        let cmp = enter_scope cmp in
        let* cmp = compile_node body cmp in
        (* Implicit returns *)
        if last_instruction_is `Pop cmp then replace_last_pop_with_return cmp ;
        (* Somehow there is not return statemet or pop in the body  *)
        let cmp =
          if not (last_instruction_is `ReturnValue cmp) then
            emit `Return cmp |> fst
          else cmp
        in
        let num_locals = cmp.symbol_table.num_definitions in
        let cmp, inst = leave_scope cmp in
        let cmp, index = add_constants (Obj.CompFunc (inst, num_locals)) cmp in
        let cmp, _ = emit (`Constant index) cmp in
        Ok cmp
    | CallExpression {func; arguments} ->
        let* cmp = compile_expression func cmp in
        let* cmp =
          List.fold_left
            (fun cmp argument ->
              Result.bind cmp (fun a -> compile_expression argument a) )
            (Ok cmp) arguments
        in
        let cmp, _ = emit (`Call (List.length arguments)) cmp in
        Ok cmp
  and compile_node node cmp =
    match node with
    | Expressionstatement exp ->
        let* cmp = compile_expression exp.expression cmp in
        let cmp, _ = emit `Pop cmp in
        Ok cmp
    | BlockStatement blck ->
        let* stmts = compile_statements blck.statements cmp in
        Ok stmts
    | Letstatement {name; value} ->
        let* cmp = compile_expression value cmp in
        let* id =
          Ast.get_ident name
          |> Option.to_result
               ~none:
                 (Code.CodeError.CustomError "expression is not an identifier ")
        in
        let st, symbol = Symbol_table.define id cmp.symbol_table in
        let cmp, _ =
          match symbol.scope with
          | GLOBAL ->
              emit (`SetGlobal symbol.index) cmp
          | LOCAL ->
              emit (`SetLocal symbol.index) cmp
        in
        Ok {cmp with symbol_table= st}
    | Returnstatement {return_value} ->
        let* cmp = compile_expression return_value cmp in
        let cmp, _ = emit `ReturnValue cmp in
        Ok cmp
  and compile_statements statement_list cmp =
    match statement_list with
    | [] ->
        Ok cmp
    | node :: tail ->
        let* cmp = compile_node node cmp in
        compile tail cmp
  in
  compile_statements nodes cmp
