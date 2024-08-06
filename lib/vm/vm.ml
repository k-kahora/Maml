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

let pop stack =
  Stack.pop_opt stack |> Option.to_result ~none:Code.CodeError.EmptyStack

let push item stack =
  let () = Stack.push item stack in
  stack

(*FIXME any inperformant functions called in here is an issue *)
(*FIXME right now this function is a mess each sub function has to end in run {vm with instructions=rest} there is a better way*)
let[@ocaml.tailcall] [@ocaml.warning "-9-11"] rec run vm =
  let open Code in
  let evaluate_opconstant instructions =
    match instructions with
    | [] ->
        Error (Code.CodeError.CustomError "Instructions Empty")
    | b1 :: b2 :: rest ->
        let constIndex = ByteFmt.int_of_hex [b1; b2] 2 in
        let constant_opt = IntMap.find_opt constIndex vm.constants in
        let* constant =
          Option.to_result ~none:(Code.CodeError.ConstantNotFound constIndex)
            constant_opt
        in
        let _stack = push constant vm.stack in
        run {vm with instructions= rest}
    | _ ->
        Error (Code.CodeError.CustomError "Not enough instructions")
  in
  let evaluate_opadd rest =
    let* right = pop vm.stack in
    let* left = pop vm.stack in
    match (left, right) with
    | Obj.Int r, Obj.Int l ->
        let _stack = push (Obj.Int (r + l)) vm.stack in
        run {vm with instructions= rest}
    | l, _ ->
        Error (Code.CodeError.ObjectNotImplemented l)
  in
  let match_opcode instructions = function
    | `OPCONSTANT ->
        evaluate_opconstant instructions
    | `OPADD ->
        evaluate_opadd instructions
  in
  match vm.instructions with
  | [] ->
      Ok vm
  | instruction :: rest ->
      let* {def} = lookup instruction in
      match_opcode rest def
