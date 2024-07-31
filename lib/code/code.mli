type byte = char

type opcode = OpConstant of int | OpAdd of int

val make : opcode -> byte list

val string_of_byte_list : byte list -> string

val read_operands : opcode -> byte list -> byte list * int

val slice : int -> 'a list -> 'a list
(** [slice start lt] [start] elements are removed from the [lt] list starting at the hd *)
