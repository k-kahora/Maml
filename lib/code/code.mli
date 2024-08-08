type byte = char

(* FIXME add lessthanorequal opcode *)
type opcode =
  [ `Constant of int
  | `JumpNotTruthy of int
  | `Jump of int
  | `Add
  | `Sub
  | `Mul
  | `Div
  | `True
  | `False
  | `Equal
  | `NotEqual
  | `GreaterThan
  | `Minus
  | `Bang
  | `Pop ]

type opcode_marker = [`OPCONSTANT | `JUMP | `JUMPNOTTRUTHY]

module CodeError : sig
  type error =
    | UnrecognizedByte of byte
    | OpCodeNotImplemented of [opcode_marker | opcode]
    | StatementNotImplemented of Ast.statement
    | ExpressionNotImplemented of Ast.expression
    | ObjectNotImplemented of Object.Obj.item
    | ConstantNotFound of int
    | StackOverflow
    | CustomError of string
    | UnknownOperator of string
    | UnsuportedType of string * Object.Obj.item
    | EmptyStack

  val equal_error : error -> error -> bool

  val pp_error : Format.formatter -> error -> unit

  val print_error : error -> unit

  val alcotest_error : error Alcotest.testable
end

type definition = {def: [opcode_marker | opcode]; length: int}

val make : opcode -> byte list

val lookup : byte -> (definition, CodeError.error) result

val string_of_byte_list : byte list -> (string, CodeError.error) result

val read_operands : [< opcode | opcode_marker] -> byte list -> int * int

module ByteFmt : sig
  val slice : int -> 'a list -> 'a list
  (** [slice start lt] [start] elements are removed from the [lt] list starting at the hd *)

  val hex_of_int : int -> int -> byte list
  (** [hex_of_int operand length] [operand] is converted into a big endian encoding of length: [length], if less thant length bytes returned are \x00, if list is greater than length the length will be ignored*)

  val int_of_hex : byte list -> int -> int

  val pp_byte_list : byte list -> string
end

val create_opcode : byte -> [> opcode]
