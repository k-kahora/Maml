type byte = char

type compiler = {instructions: byte list; constants: Object.Obj.item list}

type bytecode = {instructions': byte list; constants': Object.Obj.item list}

val new_compiler : compiler

val bytecode : compiler -> bytecode

val compile : compiler -> Ast.statement list -> (int, string) result
