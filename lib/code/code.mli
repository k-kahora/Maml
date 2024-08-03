type byte = char

module CodeError : sig
  type error =
    | UnrecognizedByte of byte
    | StatementNotImplemented of Ast.statement
    | ExpressionNotImplementd of Ast.expression

  val equal_error : error -> error -> bool

  val pp_error : Format.formatter -> error -> unit

  val alcotest_error : error Alcotest.testable
end

type opcode = [`OpConstant of int]

type opcode_marker = [`OPCONSTANT]

val make : opcode -> byte list

val string_of_byte_list : byte list -> (string, CodeError.error) result

val read_operands : [< opcode | opcode_marker] -> byte list -> int * int

module ByteFmt : sig
  val slice : int -> 'a list -> 'a list
  (** [slice start lt] [start] elements are removed from the [lt] list starting at the hd *)

  val hex_of_int : int -> int -> byte list
  (** [hex_of_int operand length] [operand] is converted into a big endian encoding of length: [length], if less thant length bytes returned are \x00, if list is greater than length the length will be ignored*)

  val int_of_hex : byte list -> int -> int
end
