open Code

type 'a program_stack = {mutable ip: int; stack: 'a option array}

val make_stack : int -> 'a program_stack

val head : 'a program_stack -> ('a, CodeError.error) result

val stack_of_list : 'a list -> 'a program_stack

val push : 'a -> 'a program_stack -> unit

val pop : 'a program_stack -> ('a option, CodeError.error) result

val read_then_increment : 'a program_stack -> ('a, CodeError.error) result

val get : int -> 'a program_stack -> ('a, CodeError.error) result

val get_err : default:'a -> int -> 'a program_stack -> 'a

val set : int -> 'a -> 'a program_stack -> (unit, CodeError.error) result

val alc_program_stack : int program_stack Alcotest.testable

val stack_head : 'a program_stack -> ('a, CodeError.error) result

val list_of_programstack : default:'a -> 'a program_stack -> 'a list
