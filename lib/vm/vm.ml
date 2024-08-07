open Object

let ( let* ) = Result.bind

let stack_size = 2048

type byte = char

module IntMap = Map.Make (Int)

type virtual_machine =
  { constants: Obj.item IntMap.t
  ; instructions: byte list
  ; last_item_poped: Obj.item
  ; stack: Obj.item Stack.t }

let new_virtual_machine byte_code =
  let open Compiler in
  { constants= byte_code.constants
  ; instructions= byte_code.instructions
  ; last_item_poped= Obj.Null
  ; stack= Stack.create () }

let[@ocaml.warning "-9"] pop_stack {stack} =
  Stack.top_opt stack |> Option.to_result ~none:Code.CodeError.EmptyStack

let pop stack =
  Stack.pop_opt stack |> Option.to_result ~none:Code.CodeError.EmptyStack

let push item stack =
  let () = Stack.push item stack in
  stack

(*FIXME any inperformant functions called in here is an issue *)
(*FIXME right now this function is a mess each sub function has to end in run {vm with instructions=rest} there is a better way*)
let[@ocaml.tailcall] [@ocaml.warning "-9-11"] rec run vm =
  let finish_run vm rest = Ok {vm with instructions= rest} in
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
        Ok {vm with instructions= rest}
    | _ ->
        Error (Code.CodeError.CustomError "Not enough instructions")
  in
  let execute_binary_operation op rest =
    let execute_binary_integer_operation left right = function
      | `OPADD ->
          left + right
      | `OPSUB ->
          left - right
      | `OPMUL ->
          left * right
      | `OPDIV ->
          left / right
    in
    let* right = pop vm.stack in
    let* left = pop vm.stack in
    match (left, right) with
    | Obj.Int l, Obj.Int r ->
        ignore
        @@ push (Obj.Int (execute_binary_integer_operation l r op)) vm.stack ;
        Ok {vm with instructions= rest}
    | l, _ ->
        Error (Code.CodeError.ObjectNotImplemented l)
  in
  let evaluate_oppop rest =
    let* a =
      Stack.pop_opt vm.stack |> Option.to_result ~none:Code.CodeError.EmptyStack
    in
    Ok {vm with last_item_poped= a; instructions= rest}
  in
  let match_opcode instructions = function
    | `OPCONSTANT ->
        evaluate_opconstant instructions
    | (`OPADD | `OPSUB | `OPMUL | `OPDIV) as op ->
        execute_binary_operation op instructions
    | `OPTRUE ->
        ignore @@ push (Obj.Bool true) vm.stack ;
        finish_run vm instructions
    | `OPFALSE ->
        ignore @@ push (Obj.Bool false) vm.stack ;
        finish_run vm instructions
    | `OPPOP ->
        evaluate_oppop instructions
    | a ->
        Error (Code.CodeError.OpCodeNotImplemented a)
  in
  match vm.instructions with
  | [] ->
      Ok vm
  | instruction :: rest ->
      let* {def} = lookup instruction in
      let* vm = match_opcode rest def in
      run vm
