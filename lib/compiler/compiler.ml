open Object
module IntMap = Map.Make (Int)

let obj_map = IntMap.empty

type byte = char

module TestObj = struct
  let compare o1 o2 =
    let open Object.Obj in
    match (o1, o2) with Int a, Int b -> Int.compare a b | _ -> -1
end

let ( let* ) = Result.bind

type emitted_instruction = {opcode: Code.opcode; position: int}

type compiler =
  { instructions: byte list
  ; index: int
  ; constants: Obj.item IntMap.t
  ; last_instruction: emitted_instruction
  ; previous_instruction: emitted_instruction }

let pp_compiler fmt cmp =
  let pp_map map =
    IntMap.fold
      (fun key value acc ->
        acc ^ Format.sprintf "%d:%s, " key (Obj.item_to_string value) )
      map "{"
  in
  Format.fprintf fmt "Instructions: %s\nConstants: %s}"
    (Code.ByteFmt.pp_byte_list cmp.instructions)
    (pp_map cmp.constants)

let equal_compiler c1 c2 =
  let instructions =
    List.compare
      (fun a b -> int_of_char a - int_of_char b)
      c1.instructions c2.instructions
    = 0
  in
  let constants =
    IntMap.compare TestObj.compare c1.constants c2.constants = 0
  in
  instructions && constants

let alcotest_compiler = Alcotest.testable pp_compiler equal_compiler

type bytecode =
  {instructions': byte list; index': int; constants': Obj.item IntMap.t}

let bytecode cmp =
  {instructions'= cmp.instructions; index'= cmp.index; constants'= cmp.constants}

let new_compiler =
  { instructions= []
  ; index= 0
  ; constants= IntMap.empty
  ; previous_instruction= {opcode= `Pop; position= 0}
  ; last_instruction= {opcode= `Pop; position= 0} }

let rec add_constants obj cmp =
  ( { cmp with
      constants= IntMap.add cmp.index obj cmp.constants
    ; index= cmp.index + 1 }
  , cmp.index )

and emit op cmp =
  let inst = Code.make op in
  let cmp, pos = add_instructions inst cmp in
  let last = {opcode= op; position= pos} in
  let cmp =
    {cmp with previous_instruction= cmp.last_instruction; last_instruction= last}
  in
  (cmp, pos)

and add_instructions inst cmp =
  let pos_new_inst = List.length cmp.instructions in
  ({cmp with instructions= cmp.instructions @ inst}, pos_new_inst)

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
  let patched_instructions = f pos new_instruction cmp.instructions in
  {cmp with instructions= patched_instructions}

(**  [change_operand op_pos operand cmp] Assumes you only replace operands of the same type and same length *)
let[@ocaml.warning "-9"] change_operand op_pos operand cmp =
  let* {def} = List.nth cmp.instructions op_pos |> Code.lookup in
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
  { cmp with
    instructions= pop_last cmp.instructions
  ; last_instruction= cmp.previous_instruction }

let last_instruction_pop cmp = cmp.last_instruction.opcode = `Pop

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
        let cmp = if last_instruction_pop cmp then remove_pop cmp else cmp in
        (* NOTE Get the position after the consquence*)
        let cmp, jump_pos = emit (`Jump 9999) cmp in
        let after_consequence_pos = List.length cmp.instructions in
        let* cmp =
          change_operand jump_not_truth_pos after_consequence_pos cmp
        in
        let* cmp =
          if Option.is_none altenative then
            let cmp, _ = emit `Null cmp in
            Ok cmp
          else
            let* cmp = compile_node (Option.get altenative) cmp in
            let cmp =
              if last_instruction_pop cmp then remove_pop cmp else cmp
            in
            Ok cmp
        in
        let after_alternative = List.length cmp.instructions in
        change_operand jump_pos after_alternative cmp
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
        let cmp, inst_pos = emit (`Constant index) cmp in
        Ok cmp
    | BooleanExpression {value} ->
        let cmp, _ = if value then emit `True cmp else emit `False cmp in
        Ok cmp
    | e ->
        Error (Code.CodeError.ExpressionNotImplemented e)
  and compile_node node cmp =
    match node with
    | Expressionstatement exp ->
        let* cmp = compile_expression exp.expression cmp in
        let cmp, _ = emit `Pop cmp in
        Ok cmp
    | BlockStatement blck ->
        let* stmts = compile_statements blck.statements cmp in
        Ok stmts
    | a ->
        Error (Code.CodeError.StatementNotImplemented node)
  and compile_statements statement_list cmp =
    match statement_list with
    | [] ->
        Ok cmp
    | node :: tail ->
        let* cmp = compile_node node cmp in
        compile tail cmp
  in
  compile_statements nodes cmp
