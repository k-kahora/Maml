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

let remove_pop cmp = failwith "remove_pop not implemented"

let[@ocaml.warning "-27-9-26"] rec compile nodes cmp =
  let open Ast in
  let rec compile_expression expr cmp =
    match expr with
    | IfExpression {condition; consquence; altenative} ->
        let* cmp = compile_expression condition cmp in
        let cmp, _ = emit (`JumpNotTruthy 9999) cmp in
        let cmp =
          if cmp.last_instruction.opcode = `Pop then remove_pop cmp else cmp
        in
        let* cons = compile_node consquence cmp in
        Ok cons
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
    | Expressionstatement {expression} ->
        print_endline "in expression" ;
        let* cmp = compile_expression expression cmp in
        let cmp, _ = emit `Pop cmp in
        Ok cmp
    | BlockStatement {statements} ->
        let* stmts = compile_statements statements cmp in
        Ok stmts
    | a ->
        Error (Code.CodeError.StatementNotImplemented a)
  and compile_statements statement_list cmp =
    match statement_list with
    | [] ->
        Ok cmp
    | node :: tail ->
        let* cmp = compile_node node cmp in
        compile tail cmp
  in
  compile_statements nodes cmp
