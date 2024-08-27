open Object

module IntMap : module type of Map.Make (Int)

type byte = char

type virtual_machine =
  { constants: Obj.item IntMap.t
  ; globals: Obj.item Program_stack.program_stack
  ; last_item_poped: Obj.item
  ; frames: Frame.frame array
  ; frame_index: int
  ; stack: Obj.item Program_stack.program_stack }

(* val last_item_popped : *)
(*   virtual_machine -> (Object.Obj.item, Code.CodeError.error) result *)

val last_item_popped : virtual_machine -> Obj.item

val new_virtual_machine : Compiler.compiler -> virtual_machine

val new_with_global_store :
     Compiler.compiler
  -> Object.Obj.item Program_stack.program_stack
  -> virtual_machine

val empty_globals : unit -> 'a Program_stack.program_stack

val run : virtual_machine -> (virtual_machine, Code.CodeError.error) result

val stack_size : int
