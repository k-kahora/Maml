open Object
module IntMap = Map.Make (Int)

let obj_map = IntMap.empty

type byte = char

let ( let* ) = Result.bind

type compiler =
  {instructions: byte list; index: int; constants: Obj.item IntMap.t}

let pp_compiler fmt cmp =
  Format.fprintf fmt "Instructions: %s\nConstants"
    (Code.ByteFmt.pp_byte_list cmp.instructions)

let equal_compiler c1 c2 =
  List.compare
    (fun a b -> int_of_char a - int_of_char b)
    c1.instructions c2.instructions
  = 0

let alcotest_compiler = Alcotest.testable pp_compiler equal_compiler

type bytecode =
  {instructions': byte list; index': int; constants': Obj.item IntMap.t}

let bytecode cmp =
  {instructions'= cmp.instructions; index'= cmp.index; constants'= cmp.constants}

let new_compiler = {instructions= []; index= 0; constants= IntMap.empty}

let rec add_constants cmp obj =
  ( { cmp with
      constants= IntMap.add cmp.index obj cmp.constants
    ; index= cmp.index + 1 }
  , cmp.index )

and emit cmp _op operands =
  let inst = Code.make (`OpConstant operands) in
  let cmp, pos = add_instructions cmp inst in
  (cmp, pos)

and add_instructions cmp inst =
  let pos_new_inst = List.length cmp.instructions in
  ({cmp with instructions= cmp.instructions @ inst}, pos_new_inst)

let[@ocaml.warning "-27-9-26"] rec compile cmp nodes =
  let open Ast in
  let rec compile_expression cmp expr =
    match expr with
    | InfixExpression {left; right} ->
        let* left_compiled = compile_expression cmp left in
        let* right_compiled = compile_expression left_compiled right in
        Ok right_compiled
    | IntegerLiteral {value} ->
        let integer = Obj.Int value in
        let cmp, index = add_constants cmp integer in
        Format.printf "index: %d" index ;
        let cmp, inst_pos = emit cmp `OPCONSTANT index in
        Ok cmp
    | e ->
        Error (Code.CodeError.ExpressionNotImplementd e)
  in
  let compile_node cmp node =
    match node with
    | Expressionstatement {expression} ->
        compile_expression cmp expression
    | a ->
        Error (Code.CodeError.StatementNotImplemented a)
  in
  let compile_statements cmp statement_list =
    match statement_list with
    | [] ->
        Ok cmp
    | node :: tail ->
        let* cmp = compile_node cmp node in
        compile cmp tail
  in
  compile_statements cmp nodes
