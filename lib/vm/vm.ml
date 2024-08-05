open Object

let ( let* ) = Result.bind

let stack_size = 2048

type byte = char

module IntMap = Map.Make (Int)

type virtual_machine =
  { constants: Obj.item IntMap.t
  ; instructions: byte list
  ; stack: Obj.item Stack.t }

let new_virtual_machine byte_code =
  let open Compiler in
  { constants= byte_code.constants
  ; instructions= byte_code.instructions
  ; stack= Stack.create () }

let push constants item stack =
  let* constant =
    IntMap.find_opt item constants
    |> Option.to_result ~none:(Code.CodeError.ConstantNotFound item)
  in
  Ok (Stack.push constant stack)

(*FIXME any inperformant functions called in here is an issue *)
let[@ocaml.tailcall] [@ocaml.warning "-9-11"] rec run vm =
  let open Code in
  let evaluate_opconstant instructions =
    match instructions with
    | [] ->
        Error (Code.CodeError.CustomError "Instructions Empty")
    | b1 :: b2 :: rest ->
        let constIndex = ByteFmt.int_of_hex [b1; b2] 2 in
        let* _ = push vm.constants constIndex vm.stack in
        run {vm with instructions= rest}
    | _ ->
        Error (Code.CodeError.CustomError "Not enough instructions")
  in
  let evaluate_opadd () =
    Error (Code.CodeError.CustomError "OPADD not implemented in VM")
  in
  let match_opcode instructions = function
    | `OPCONSTANT ->
        evaluate_opconstant instructions
    | `OPADD ->
        evaluate_opadd ()
  in
  match vm.instructions with
  | [] ->
      Ok vm
  | instruction :: rest ->
      let* {def} = lookup instruction in
      match_opcode rest def
