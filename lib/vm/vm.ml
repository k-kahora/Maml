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

module VM_Helpers = struct
  open Code

  let finish_run rest vm = Ok {vm with instructions= rest}

  let evaluate_opconstant instructions vm =
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

  let execute_binary_operation op rest vm =
    let execute_binary_integer_operation left right = function
      | `Add ->
          left + right
      | `Sub ->
          left - right
      | `Mul ->
          left * right
      | `Div ->
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

  let execute_bool value rest vm =
    ignore @@ push (Obj.Bool value) vm.stack ;
    finish_run rest vm

  let execute_comparison op rest vm =
    let execute_primitive_compare left right = function
      | `Equal ->
          left = right
      | `NotEqual ->
          left <> right
      | `GreaterThan ->
          left > right
    in
    let* right = pop vm.stack in
    let* left = pop vm.stack in
    let evaluate_compare l r op =
      let value = execute_primitive_compare l r op in
      ignore @@ push (Obj.Bool value) vm.stack ;
      finish_run rest vm
    in
    match (left, right) with
    | Obj.Int l, Int r ->
        evaluate_compare l r op
    | Bool l, Bool r ->
        evaluate_compare l r op
    | l, _ ->
        Error (Code.CodeError.ObjectNotImplemented l)

  let evaluate_oppop rest vm =
    let* a =
      Stack.pop_opt vm.stack |> Option.to_result ~none:Code.CodeError.EmptyStack
    in
    {vm with last_item_poped= a} |> finish_run rest

  let execute_minus rest vm =
    let* operand = pop vm.stack in
    let* operand =
      match operand with
      | Obj.Int i ->
          Ok (Obj.Int (i * -1))
      | a ->
          Error (CodeError.UnsuportedType ("negation", a))
    in
    ignore @@ push operand vm.stack ;
    finish_run rest vm

  let execute_bang rest vm =
    let* operand = pop vm.stack in
    let* operand =
      match operand with
      | Obj.Bool b ->
          Ok (Obj.Bool (not b))
      | _ ->
          Ok (Obj.Bool false)
    in
    ignore @@ push operand vm.stack ;
    finish_run rest vm
end

(*FIXME any inperformant functions called in here is an issue *)
(*FIXME right now this function is a mess each sub function has to end in run {vm with instructions=rest} there is a better way*)
let[@ocaml.tailcall] [@ocaml.warning "-9-11"] rec run vm =
  let open Code in
  let match_opcode instructions = function
    | `Constant _ | `OPCONSTANT ->
        VM_Helpers.evaluate_opconstant instructions vm
    | (`Add | `Sub | `Mul | `Div) as op ->
        VM_Helpers.execute_binary_operation op instructions vm
    | `True ->
        VM_Helpers.execute_bool true instructions vm
    | `False ->
        VM_Helpers.execute_bool false instructions vm
    | `Pop ->
        VM_Helpers.evaluate_oppop instructions vm
    | (`Equal | `NotEqual | `GreaterThan) as op ->
        VM_Helpers.execute_comparison op instructions vm
    | `Bang ->
        VM_Helpers.execute_bang instructions vm
    | `Minus ->
        VM_Helpers.execute_minus instructions vm
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
