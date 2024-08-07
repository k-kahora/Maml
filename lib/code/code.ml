type byte = char

(* FIXME add lessthanorequal opcode *)
type opcode =
  [ `OpConstant of int
  | `OpAdd
  | `OpSub
  | `OpMul
  | `OpDiv
  | `OpTrue
  | `OpFalse
  | `OpEqual
  | `OpNotEqual
  | `OpGreaterThan
  | `OpPop ]

type opcode_marker =
  [ `OPCONSTANT
  | `OPADD
  | `OPPOP
  | `OPSUB
  | `OPMUL
  | `OPDIV
  | `OPTRUE
  | `OPFALSE
  | `OPEQUAL
  | `OPGREATERTHAN
  | `OPNOTEQUAL ]

let operand_name = function
  | `OPCONSTANT | `OpConstant _ ->
      "OpConstant"
  | `OPADD | `OpAdd ->
      "OpAdd"
  | `OPPOP | `OpPop ->
      "OpPop"
  | `OPSUB | `OpSub ->
      "OpSub"
  | `OPMUL | `OpMul ->
      "OpMul"
  | `OPDIV | `OpDiv ->
      "OpDiv"
  | `OPTRUE | `OpTrue ->
      "OpTrue"
  | `OPFALSE | `OpFalse ->
      "OpFalse"
  | `OpEqual | `OPEQUAL ->
      "OpEqual"
  | `OpGreaterThan | `OPGREATERTHAN ->
      "OpGreaterThan"
  | `OpNotEqual | `OPNOTEQUAL ->
      "OpNotEqual"

module CodeError = struct
  type error =
    | UnrecognizedByte of byte
    | OpCodeNotImplemented of opcode_marker
    | StatementNotImplemented of Ast.statement
    | ExpressionNotImplemented of Ast.expression
    | ObjectNotImplemented of Object.Obj.item
    | ConstantNotFound of int
    | StackOverflow
    | CustomError of string
    | UnknownOperator of string
    | EmptyStack

  let equal_error e1 e2 = e1 = e2

  let format_helper = Format.fprintf

  let pp_error fmt = function
    | UnrecognizedByte b ->
        format_helper fmt "UnrecognizedByte 0x%02X" (int_of_char b)
    | OpCodeNotImplemented op ->
        format_helper fmt "OpCodeNotImplemented: %s" (operand_name op)
    | StatementNotImplemented stmt ->
        format_helper fmt "StatementNotImplemented %s"
          (Ast.statement_str_debug stmt)
    | ObjectNotImplemented obj ->
        format_helper fmt "StatementNotImplemented %s"
          (Object.Obj.object_string obj)
    | ExpressionNotImplemented expr ->
        format_helper fmt "ExpressionNotImplementd %s"
          (Ast.expression_str_debug expr)
    | StackOverflow ->
        format_helper fmt "Stack Overflow"
    | UnknownOperator operator ->
        format_helper fmt "UnknownOperator: %s" operator
    | EmptyStack ->
        format_helper fmt "Empty Stack"
    | ConstantNotFound index ->
        format_helper fmt "ConstantNotFound: at index %d" index
    | CustomError err ->
        format_helper fmt "CustomError: %s" err

  let print_error error =
    let error_str = Format.asprintf "%a" pp_error error in
    print_endline error_str

  let alcotest_error = Alcotest.testable pp_error equal_error
end

module ByteFmt = struct
  let slice start lst = List.filteri (fun i _ -> i >= start) lst

  let int_of_hex byte_list length =
    let[@ocaml.tailcall] rec helper ~count acc shift = function
      | [] ->
          acc
      | _ when count = 0 ->
          acc
      | b :: tl ->
          let byte = int_of_char b in
          let acc = byte lor (acc lsl shift) in
          helper ~count:(count - 1) acc 8 tl
    in
    (* let starting_index = *)
    (*   List.find_index (fun a -> a <> '\x00') byte_list *)
    (*   |> Option.value ~default:0 *)
    (* in *)
    (* let byte_list = slice starting_index byte_list in *)
    helper 0 0 byte_list ~count:length

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
  let pp_byte by = Format.sprintf "0x%02X," (int_of_char by)

  let pp_byte_list byte_list =
    let print_byte acc nxt = Format.sprintf "%s %s" acc (pp_byte nxt) in
    "[" ^ List.fold_left print_byte "" byte_list ^ "]"
end

open ByteFmt

let ( let* ) = Result.bind

(* first argument is always length*)

type definition = {def: opcode_marker; length: int}

let opcode_length = function
  | `OPCONSTANT | `OpConstant _ ->
      2
  | `OpAdd
  | `OPADD
  | `OpPop
  | `OPPOP
  | `OpSub
  | `OPSUB
  | `OpMul
  | `OPMUL
  | `OpDiv
  | `OPDIV
  | `OPTRUE
  | `OpTrue
  | `OPFALSE
  | `OpFalse
  | `OPEQUAL
  | `OpEqual
  | `OPNOTEQUAL
  | `OpNotEqual
  | `OPGREATERTHAN
  | `OpGreaterThan ->
      0

let lookup = function
  | '\x01' ->
      let mark = `OPCONSTANT in
      Ok {def= mark; length= opcode_length mark}
  | '\x02' ->
      Ok {def= `OPADD; length= 0}
  | '\x03' ->
      Ok {def= `OPPOP; length= 0}
  | '\x04' ->
      Ok {def= `OPSUB; length= 0}
  | '\x05' ->
      Ok {def= `OPDIV; length= 0}
  | '\x06' ->
      Ok {def= `OPMUL; length= 0}
  | '\x07' ->
      Ok {def= `OPTRUE; length= 0}
  | '\x08' ->
      Ok {def= `OPFALSE; length= 0}
  | '\x09' ->
      Ok {def= `OPEQUAL; length= 0}
  | '\x0A' ->
      Ok {def= `OPNOTEQUAL; length= 0}
  | '\x0B' ->
      Ok {def= `OPGREATERTHAN; length= 0}
  | a ->
      Error (CodeError.UnrecognizedByte a)

(** [make op] converts an opcode into a list of bytes of varying length *)
let make op =
  let length = opcode_length op in
  match op with
  | `OpConstant operand ->
      '\x01' :: hex_of_int operand length
  | `OpAdd ->
      ['\x02']
  | `OpPop ->
      ['\x03']
  | `OpSub ->
      ['\x04']
  | `OpDiv ->
      ['\x05']
  | `OpMul ->
      ['\x06']
  | `OpTrue ->
      ['\x07']
  | `OpFalse ->
      ['\x08']
  | `OpEqual ->
      ['\x09']
  | `OpNotEqual ->
      ['\x0A']
  | `OpGreaterThan ->
      ['\x0B']

let[@ocaml.warning "-27"] read_operands op instructions =
  let length = opcode_length op in
  match instructions with [] -> (0, 0) | ls -> (int_of_hex ls length, length)

(* NOTE I believe I have almost perfected this function  *)
let[@ocaml.warning "-27"] string_of_byte_list byte_list =
  let format_operands acc index def operands =
    match operands with
    | 0 ->
        Format.sprintf "%s\n%04d %s" acc index (operand_name def)
    | a ->
        Format.sprintf "%s\n%04d %s %d" acc index (operand_name def) a
  in
  let[@ocaml.tailcall] rec helper ~lst ~index acc =
    match lst with
    | [] ->
        Ok acc
    | b :: tail as _list ->
        let* {def; length} = lookup b in
        (* Format.printf "Index is %d" idx ; *)
        let operands, bytes_read = read_operands def tail in
        let acc = format_operands acc index def operands in
        let new_list = slice bytes_read tail in
        helper ~lst:new_list ~index:(index + bytes_read + 1) acc
  in
  helper ~lst:byte_list ~index:0 ""

let create_opcode opcode = `OpConstant (int_of_hex [opcode] 1)
