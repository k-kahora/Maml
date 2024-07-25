type byte = char

type opcode = OpConstant

(* Define a GADT to represent lists of chars with a given length *)

(* I want to limit it so that  *)

module Byte = struct
  let print_byte b = Format.printf "%2X" (int_of_char b)

  let print_bytes_seq b = Format.printf "Bytes: " ; List.iter print_byte b

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
end

let byte_of_opcode = function OpConstant -> '\x01'

let make opcode num =
  match opcode with
  | OpConstant as op ->
      [byte_of_opcode op] @ Byte.int_of_hex num
