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
