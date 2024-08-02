type byte = char

module CodeError = struct
  type error = UnrecognizedByte of byte

  let equal_error e1 e2 = e1 = e2

  let pp_error fmt = function
    | UnrecognizedByte b ->
        Format.fprintf fmt "UnrecognizedByte 0x%02X" (int_of_char b)

  let alcotest_error = Alcotest.testable pp_error equal_error
end

module ByteFmt = struct
  let slice start lst = List.filteri (fun i _ -> i >= start) lst

  let int_of_hex byte_list length =
    let[@ocaml.tailcall] rec helper count acc shift = function
      | [] ->
          acc
      | _ when count = 0 ->
          acc
      | b :: tl ->
          let byte = int_of_char b in
          let acc = byte lor (acc lsl shift) in
          helper (count - 1) acc (shift + 8) tl
    in
    (* let starting_index = *)
    (*   List.find_index (fun a -> a <> '\x00') byte_list *)
    (*   |> Option.value ~default:0 *)
    (* in *)
    (* let byte_list = slice starting_index byte_list in *)
    helper length 0 0 byte_list

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

  (* FIXME This a ugly function *)
  let print_byte_list byte_list =
    let print_byte acc nxt =
      acc ^ Format.sprintf "0x%02X, " (int_of_char nxt)
    in
    print_endline ("[" ^ List.fold_left print_byte "" byte_list ^ "]")
end

open ByteFmt

let ( let* ) = Result.bind

(* first argument is always length*)

type opcode = [`OpConstant of int]

type opcode_marker = [`OPCONSTANT]

type definition = {def: opcode_marker; length: int}

let opcode_length = function `OPCONSTANT | `OpConstant _ -> 2

let lookup = function
  | '\x01' ->
      let mark = `OPCONSTANT in
      Ok {def= mark; length= opcode_length mark}
  | a ->
      Error (CodeError.UnrecognizedByte a)

(** [make op] converts an opcode into a list of bytes of varying length *)
let make op =
  let length = opcode_length op in
  match op with `OpConstant operand -> '\x01' :: hex_of_int operand length

let[@ocaml.warning "-27"] read_operands op instructions =
  let length = opcode_length op in
  match instructions with
  | [] ->
      failwith "Turn this into a result"
  | _ :: b ->
      (int_of_hex b length, length)

(* NOTE I believe I have almost perfected this function  *)
let[@ocaml.warning "-27"] string_of_byte_list byte_list =
  let format_operands operands = "" in
  print_byte_list byte_list ;
  let[@ocaml.tailcall] rec helper ~lst acc =
    match lst with
    | [] ->
        Ok acc
    | b :: tail ->
        let* {def; length} = lookup b in
        (* Format.printf "Index is %d" idx ; *)
        print_byte_list tail ;
        let operands, bytes_read = read_operands def tail in
        let acc = acc ^ format_operands operands in
        let new_list = slice bytes_read tail in
        helper ~lst:new_list acc
  in
  helper ~lst:byte_list ""
