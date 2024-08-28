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
  ; scope_index: int
  ; constants: Obj.item IntMap.t
  ; symbol_table: Symbol_table.symbol_table
  ; scopes: compilation_scope array }

val current_instructions : compiler -> byte list

val new_compiler : unit -> compiler

val new_with_state :
  int -> Symbol_table.symbol_table -> Object.Obj.item IntMap.t -> compiler

val empty_constants : unit -> 'a IntMap.t

val compile :
  Ast.statement list -> compiler -> (compiler, Code.CodeError.error) result

val emit : Code.opcode -> compiler -> compiler * int

val alcotest_compiler : compiler Alcotest.testable

val enter_scope : compiler -> compiler

val leave_scope : compiler -> compiler * byte list
(** [compile compiler program] The [program] is first evaluated as a list of statements and each statement is then evaluated on its own type going deeper into the AST returns a completed compiler *)
(* val compile : compiler -> Ast.program -> (int, error) result *)
