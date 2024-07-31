type byte = char

(* first argument is always length*)
type opcode = OpConstant of int | OpAdd of int

let opcode_length = function OpConstant _ -> 2 | OpAdd _ -> 0

(** [hex_of_int operand length] [operand] is converted into a big endian encoding of length: [length], if less thant length bytes returned are \x00, if list is greater than length the length will be ignored*)
let hex_of_int operand length =
  let rec helper acc = function
    | 0 ->
        acc
    | operand ->
        let byte = char_of_int (operand land 0xFF) in
        helper (byte :: acc) (operand lsr 8)
  in
  let lst = helper [] operand in
  let lst_length = List.length lst in
  if lst_length >= length then lst
  else List.init (length - lst_length) (fun _ -> '\x00') @ lst

(** [make op] converts an opcode into a list of bytes of varying length *)
let make op =
  let length = opcode_length op in
  match op with
  | OpConstant operand ->
      '\x01' :: hex_of_int operand length
  | OpAdd _ ->
      []

let[@ocaml.warning "-27"] string_of_byte_list byte_list = ""

let slice start lst = List.filteri (fun i _ -> i >= start) lst

let[@ocaml.warning "-27"] read_operands op instructions = ([], 0)
