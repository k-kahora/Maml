open Object
module IntMap = Map.Make (Int)

let obj_map = IntMap.empty

type byte = char

let ( let* ) = Result.bind

type compiler =
  {instructions: byte list; index: int; constants: Obj.item IntMap.t}

type bytecode =
  {instructions': byte list; index': int; constants': Obj.item IntMap.t}

let bytecode _compiler = {instructions'= []; index'= 0; constants'= IntMap.empty}

let new_compiler = {instructions= []; index= 0; constants= IntMap.empty}

let add_constants cmp obj =
  { cmp with
    constants= IntMap.add cmp.index obj cmp.constants
  ; index= cmp.index + 1 }

let[@ocaml.warning "-27-9-26"] rec compile cmp nodes =
  let open Ast in
  let rec compile_expression cmp expr =
    match expr with
    | InfixExpression {left; right} ->
        let* left_compiled = compile_expression cmp left in
        let* right_compiled = compile_expression cmp right in
        Ok cmp
    | IntegerLiteral {value} ->
        let integer = Obj.Int value in
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
    | h :: tail ->
        let* cmp = compile_node cmp h in
        compile cmp tail
  in
  compile_statements cmp nodes
