(* open StdLabels *)

type byte = char

type opcode = OpConstant | OpAdd

type definition = {name: string; operand_width: int list}

let ( let* ) = Result.bind

let lookups = function
  | OpConstant ->
      {name= "OpConstant"; operand_width= [2]}
  | OpAdd ->
      assert false

(* Define a GADT to represent lists of chars with a given length *)

(* I want to limit it so that  *)

module Byte = struct
  let print_byte b = Format.sprintf "%#2X" (int_of_char b)

  let to_string b = List.map print_byte b |> List.fold_left ( ^ ) ""

  let int_of_hex : int -> byte list =
   fun num ->
    let rec int_of_hex_helper num10 acc =
      match num10 with
      | 0 ->
          acc
      | x ->
          let byte = char_of_int (x land 0xFF) in
          int_of_hex_helper (num10 lsr 8) (byte :: acc)
    in
    int_of_hex_helper num []

  let hex_of_int : byte list -> int =
   fun bytes ->
    let rec helper bytes acc shift =
      match bytes with
      | [] ->
          acc
      | b :: tl ->
          let num = int_of_char b in
          let acc = acc lor (num lsl shift) in
          helper tl acc (shift + 8)
    in
    helper (List.rev bytes) 0 0

  let byte = function OpConstant -> '\x01'

  let string_of_byte _b = ""
end

(* let lookup = function *)
(*   | '\x01' -> *)
(*       OpConstant *)
(*   | b -> *)
(*       failwith @@ Format.sprintf "%#2X" (int_of_char b) *)

(* let byte_of_opcode = function OpConstant -> '\x01' *)

let make opcode operands =
  let code = lookups opcode in
  let _instruction_length = List.fold_left ( + ) 0 code.operand_width in
  Byte.byte opcode
  :: List.fold_left
       (fun acc operand -> List.append acc (Byte.int_of_hex operand))
       [] operands

let slice start lst = List.filteri (fun i _ -> i >= start) lst

let read_operands definition ins =
  List.fold_left
    (fun (offset, acc) width ->
      let create_tuple () =
        (offset + width, Byte.hex_of_int (slice offset ins) :: acc)
      in
      match width with 2 -> create_tuple () )
    (0, []) definition.operand_width
