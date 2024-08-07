open Object

module IntMap : module type of Map.Make (Int)

type byte = char

type virtual_machine =
  { constants: Obj.item IntMap.t
  ; instructions: byte list
  ; last_item_poped: Obj.item
  ; stack: Obj.item Stack.t }

val pop_stack :
  virtual_machine -> (Object.Obj.item, Code.CodeError.error) result

val new_virtual_machine : Compiler.compiler -> virtual_machine

val run : virtual_machine -> (virtual_machine, Code.CodeError.error) result
