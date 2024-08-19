open Object

type byte = char

module IntMap : module type of Map.Make (Int)

type emitted_instruction = {opcode: Code.opcode; position: int}

type compilation_scope =
  { instructions: byte list
  ; last_instruction: emitted_instruction
  ; previous_instruction: emitted_instruction }

type compiler =
  { index: int
  ; constants: Obj.item IntMap.t
  ; scopes: compilation_scope Program_stack.program_stack
  ; symbol_table: Symbol_table.symbol_table }

val current_instructions : compiler -> byte list

val new_compiler : compiler

val new_with_state :
  Symbol_table.symbol_table -> Object.Obj.item IntMap.t -> compiler

val empty_symbol_table_and_constants :
  unit -> Symbol_table.symbol_table * 'a IntMap.t

val compile :
  Ast.statement list -> compiler -> (compiler, Code.CodeError.error) result

val alcotest_compiler : compiler Alcotest.testable

(* val compile : compiler -> Ast.program -> (int, error) result *)
(** [compile compiler program] The [program] is first evaluated as a list of statements and each statement is then evaluated on its own type going deeper into the AST returns a completed compiler *)
