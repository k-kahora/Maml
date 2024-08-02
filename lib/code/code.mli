type byte = char

module CodeError : sig
  type error = UnrecognizedByte of byte

  val equal_error : error -> error -> bool

  val pp_error : Format.formatter -> error -> unit

  val alcotest_error : error Alcotest.testable
end

type opcode = [`OpConstant of int]

val make : opcode -> byte list

val string_of_byte_list : byte list -> (string, CodeError.error) result

val read_operands : byte list -> int list * int

module ByteFmt : sig
  val slice : int -> 'a list -> 'a list
  (** [slice start lt] [start] elements are removed from the [lt] list starting at the hd *)
end
