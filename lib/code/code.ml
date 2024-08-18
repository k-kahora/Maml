type byte = char

(* FIXME add lessthanorequal opcode *)
type opcode =
  [ `Constant of int
  | `JumpNotTruthy of int
  | `Jump of int
  | `GetGlobal of int
  | `SetGlobal of int
  | `Array of int
  | `Hash of int
  | `Add
  | `Sub
  | `Null
  | `Mul
  | `Div
  | `True
  | `False
  | `Equal
  | `NotEqual
  | `NotEqual
  | `GreaterThan
  | `Minus
  | `Bang
  | `Index
  | `Pop ]

type opcode_marker =
  [`CONSTANT | `JUMP | `JUMPNOTTRUTHY | `GETGLOBAL | `SETGLOBAL | `ARRAY | `HASH]

let operand_name = function
  | `Constant _ | `CONSTANT ->
      "Constant"
  | `JumpNotTruthy _ | `JUMPNOTTRUTHY ->
      "JumpNotTruthy"
  | `Jump _ | `JUMP ->
      "Jump"
  | `GetGlobal _ | `GETGLOBAL ->
      "GetGlobal"
  | `SetGlobal _ | `SETGLOBAL ->
      "SetGlobal"
  | `Array _ | `ARRAY ->
      "Array"
  | `Hash _ | `HASH ->
      "Hash"
  | `Add ->
      "Add"
  | `Pop ->
      "Pop"
  | `Sub ->
      "Sub"
  | `Mul ->
      "Mul"
  | `Div ->
      "Div"
  | `True ->
      "True"
  | `False ->
      "False"
  | `Equal ->
      "Equal"
  | `GreaterThan ->
      "GreaterThan"
  | `Minus ->
      "Minus"
  | `Null ->
      "Null"
  | `Bang ->
      "Bang"
  | `NotEqual ->
      "NotEqual"
  | `Index ->
      "Index"

let infix_operand_string = function
  | `Add ->
      "+"
  | `Sub ->
      "-"
  | `Mul ->
      "*"
  | `Div ->
      "/"

module CodeError = struct
  type error =
    | UnrecognizedByte of byte
    | OpCodeNotImplemented of [opcode_marker | opcode]
    | StatementNotImplemented of Ast.statement
    | ExpressionNotImplemented of Ast.expression
    | ObjectNotImplemented of Object.Obj.item
    | ConstantNotFound of int
    | StackOverflow
    | CustomError of string
    | UnknownOperator of string
    | UnsuportedType of string * Object.Obj.item
    | UnsuportedOperator of Object.Obj.item * string
    | SymbolNotFound of string * string
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
        format_helper fmt "ObjectNotImplemented %s"
          (Object.Obj.object_string obj)
    | ExpressionNotImplemented expr ->
        format_helper fmt "ExpressionNotImplementd %s"
          (Ast.expression_str_debug expr)
    | StackOverflow ->
        format_helper fmt "Stack Overflow"
    | UnknownOperator operator ->
        format_helper fmt "UnknownOperator: %s" operator
    | UnsuportedType (for_type, obj) ->
        format_helper fmt "UnsuportedType: for %s -> %s" for_type
          (Object.Obj.item_to_string obj)
    | EmptyStack ->
        format_helper fmt "EmptyStackError"
    | ConstantNotFound index ->
        format_helper fmt "ConstantNotFound: at index %d" index
    | UnsuportedOperator (obj, op) ->
        format_helper fmt "UnsuportedOperator: %s do not support %s"
          (Object.Obj.object_string obj)
          op
    | SymbolNotFound (caller, symbol) ->
        format_helper fmt "SymbolNotFound: Symbol -> %s; Called From %s" symbol
          caller
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

type definition = {def: [opcode_marker | opcode]; length: int}

let opcode_length = function
  | `CONSTANT
  | `Constant _
  | `Jump _
  | `JUMP
  | `JumpNotTruthy _
  | `SETGLOBAL
  | `SetGlobal _
  | `GETGLOBAL
  | `GetGlobal _
  | `JUMPNOTTRUTHY
  | `Hash _
  | `HASH
  | `Array _
  | `ARRAY ->
      2
  | `Pop
  | `Sub
  | `Add
  | `Mul
  | `Div
  | `True
  | `False
  | `Equal
  | `NotEqual
  | `Minus
  | `Bang
  | `Null
  | `Index
  | `GreaterThan ->
      0

let marker_to_opcode operand = function
  | `CONSTANT ->
      Ok (`Constant operand)
  | `JUMP ->
      Ok (`Jump operand)
  | `JUMPNOTTRUTHY ->
      Ok (`JumpNotTruthy operand)
  | `ARRAY ->
      Ok (`Array operand)
  | `HASH ->
      Ok (`Hash operand)
  | a ->
      Error (CodeError.OpCodeNotImplemented a)

let lookup_opcode = function
  | '\x01' ->
      Ok `CONSTANT
  | '\x02' ->
      Ok `Pop
  | '\x03' ->
      Ok `Sub
  | '\x04' ->
      Ok `Add
  | '\x05' ->
      Ok `Mul
  | '\x06' ->
      Ok `Div
  | '\x07' ->
      Ok `True
  | '\x08' ->
      Ok `False
  | '\x09' ->
      Ok `Equal
  | '\x0A' ->
      Ok `NotEqual
  | '\x0B' ->
      Ok `GreaterThan
  | '\x0C' ->
      Ok `Minus
  | '\x0D' ->
      Ok `Bang
  | '\x0E' ->
      Ok `JUMP
  | '\x0F' ->
      Ok `JUMPNOTTRUTHY
  | '\x10' ->
      Ok `Null
  | '\x11' ->
      Ok `SETGLOBAL
  | '\x12' ->
      Ok `GETGLOBAL
  | '\x13' ->
      Ok `ARRAY
  | '\x14' ->
      Ok `HASH
  | '\x15' ->
      Ok `Index
  | a ->
      Error (CodeError.UnrecognizedByte a)

let lookup_byte = function
  | `CONSTANT | `Constant _ ->
      '\x01'
  | `Pop ->
      '\x02'
  | `Sub ->
      '\x03'
  | `Add ->
      '\x04'
  | `Mul ->
      '\x05'
  | `Div ->
      '\x06'
  | `True ->
      '\x07'
  | `False ->
      '\x08'
  | `Equal ->
      '\x09'
  | `NotEqual ->
      '\x0A'
  | `GreaterThan ->
      '\x0B'
  | `Minus ->
      '\x0C'
  | `Bang ->
      '\x0D'
  | `Jump _ | `JUMP ->
      '\x0E'
  | `JumpNotTruthy _ | `JUMPNOTTRUTHY ->
      '\x0F'
  | `Null ->
      '\x10'
  | `Index ->
      '\x15'
  | `SetGlobal _ | `SETGLOBAL ->
      '\x11'
  | `GetGlobal _ | `GETGLOBAL ->
      '\x12'
  | `Array _ | `ARRAY ->
      '\x13'
  | `Hash _ | `HASH ->
      '\x14'

let lookup byte =
  let* opcode = lookup_opcode byte in
  Ok {def= opcode; length= opcode_length opcode}

(** [make op] converts an opcode into a list of bytes of varying length *)
let make op =
  let length = opcode_length op in
  let lb = lookup_byte in
  let convert op operand length = lb op :: hex_of_int operand length in
  match op with
  | `Constant operand ->
      convert op operand length
  | `JumpNotTruthy operand ->
      convert op operand length
  | `Jump operand ->
      convert op operand length
  | `SetGlobal operand ->
      convert op operand length
  | `GetGlobal operand ->
      convert op operand length
  | `Array operand ->
      convert op operand length
  | `Hash operand ->
      convert op operand length
  | a ->
      [lb a]

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
        let operands, bytes_read = read_operands def tail in
        let acc = format_operands acc index def operands in
        let new_list = slice bytes_read tail in
        helper ~lst:new_list ~index:(index + bytes_read + 1) acc
  in
  helper ~lst:byte_list ~index:0 ""

let create_opcode opcode = `Constant (int_of_hex [opcode] 1)
