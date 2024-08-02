type byte = char

(* first argument is always length*)

type opcode = OpConstant of int option | OpAdd of int option

let opcode_length = function OpConstant _ -> 2 | OpAdd _ -> 0

let slice start lst = List.filteri (fun i _ -> i >= start) lst

let int_of_hex byte_list =
  let[@ocaml.tailcall] rec helper acc shift = function
    | [] ->
        acc
    | b :: tl ->
        let byte = int_of_char b in
        let acc = byte lor (acc lsl shift) in
        helper acc (shift + 8) tl
  in
  let starting_index =
    List.find_index (fun a -> a <> '\x00') byte_list |> Option.value ~default:0
  in
  let byte_list = slice starting_index byte_list in
  helper 0 0 byte_list

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

let lookup = function
| '\x01' -> OpConstant 

let[@ocaml.warning "-27"] string_of_byte_list byte_list = 
  let opcod_length  = 


let[@ocaml.warning "-27"] read_operands op instructions =
  let _ = opcode_length op in
  match instructions with
  | [] ->
      failwith "Turn this into a result"
  | a :: b ->
      ([int_of_hex b], List.length b)
