open Code
open Object

type compiler = {instructions: byte list; constants: Obj.item list}

type bytecode = {instructions_byte: byte list; constants_byte: Obj.item list}

let new_compiler = {instructions= []; constants= []}

let compile compiler tree =
  let _ = (compiler, tree) in
  new_compiler

let bytecode_compiler {instructions; constants} =
  {instructions_byte= instructions; constants_byte= constants}

(* // compiler/compiler.go *)

(* package compiler *)

(* import ( *)
(*     "monkey/ast" *)
(*     "monkey/code" *)
(*     "monkey/object" *)
(* ) *)

(* type Compiler struct { *)
(*     instructions code.Instructions *)
(*     constants    []object.Object *)
(* } *)

(* func New() *Compiler { *)
(*     return &Compiler{ *)
(*         instructions: code.Instructions{}, *)
(*         constants:    []object.Object{}, *)
(*     } *)
(* } *)

(* func (c *Compiler) Compile(node ast.Node) error { *)
(*     return nil *)
(* } *)

(* func (c *Compiler) Bytecode() *Bytecode { *)
(*     return &Bytecode{ *)
(*         Instructions: c.instructions, *)
(*         Constants:    c.constants, *)
(*     } *)
(* } *)

(* type Bytecode struct { *)
(*     Instructions code.Instructions *)
(*     Constants    []object.Object *)
(* } *)
