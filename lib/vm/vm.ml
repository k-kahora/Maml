open Object

let ( let* ) = Result.bind

let stack_size = 2048

type byte = char

module IntMap = Map.Make (Int)

type virtual_machine =
  { constants: Obj.item IntMap.t
  ; instructions: byte Program_stack.program_stack
  ; last_item_poped: Obj.item
  ; stack: Obj.item Program_stack.program_stack }

let new_virtual_machine byte_code =
  let open Compiler in
  { constants= byte_code.constants
  ; instructions= Program_stack.stack_of_list byte_code.instructions
  ; last_item_poped= Obj.Null
  ; stack= Program_stack.make_stack stack_size }

(* Program_stack.pop *)

let pop vm =
  let* item = Program_stack.pop vm.stack in
  Option.fold ~none:(Error (Code.CodeError.CustomError "Poped a None Value"))
    ~some:(fun a -> Ok a)
    item

let push item vm = Program_stack.push item vm.stack

module VM_Helpers = struct
  open Code

  let finish_run vm = Ok vm

  let evaluate_opconstant vm =
    let* b1 = Program_stack.read_then_increment vm.instructions in
    let* b2 = Program_stack.read_then_increment vm.instructions in
    let constIndex = ByteFmt.int_of_hex [b1; b2] 2 in
    let constant_opt = IntMap.find_opt constIndex vm.constants in
    let* constant =
      Option.to_result ~none:(Code.CodeError.ConstantNotFound constIndex)
        constant_opt
    in
    push constant vm ; finish_run vm

  let execute_binary_operation op vm =
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
    let* right = pop vm in
    let* left = pop vm in
    match (left, right) with
    | Obj.Int l, Obj.Int r ->
        push (Obj.Int (execute_binary_integer_operation l r op)) vm ;
        finish_run vm
    | l, _ ->
        Error (Code.CodeError.ObjectNotImplemented l)

  let execute_bool value vm = push (Obj.Bool value) vm ; finish_run vm

  let execute_comparison op vm =
    let execute_primitive_compare left right = function
      | `Equal ->
          left = right
      | `NotEqual ->
          left <> right
      | `GreaterThan ->
          left > right
    in
    let* right = pop vm in
    let* left = pop vm in
    let evaluate_compare l r op =
      let value = execute_primitive_compare l r op in
      ignore @@ push (Obj.Bool value) vm ;
      finish_run vm
    in
    match (left, right) with
    | Obj.Int l, Int r ->
        evaluate_compare l r op
    | Bool l, Bool r ->
        evaluate_compare l r op
    | l, _ ->
        Error (Code.CodeError.ObjectNotImplemented l)

  let evaluate_oppop vm =
    let* a = pop vm in
    {vm with last_item_poped= a} |> finish_run

  let execute_minus vm =
    let* operand = pop vm in
    let* operand =
      match operand with
      | Obj.Int i ->
          Ok (Obj.Int (i * -1))
      | a ->
          Error (CodeError.UnsuportedType ("negation", a))
    in
    push operand vm ; finish_run vm

  let execute_bang vm =
    let* operand = pop vm in
    let* operand =
      match operand with
      | Obj.Bool b ->
          Ok (Obj.Bool (not b))
      | _ ->
          Ok (Obj.Bool false)
    in
    push operand vm ; finish_run vm

  (* NOTE YOu were tired and keep typing stack instead of instruction ip, SWITCH stack back to a stack and not your custom BS *)
  let evaluate_jump vm =
    let* b1 = Program_stack.read_then_increment vm.instructions in
    let* b2 = Program_stack.read_then_increment vm.instructions in
    let jump_pos = ByteFmt.int_of_hex [b1; b2] 2 in
    vm.instructions.ip <- jump_pos ;
    finish_run vm

  let truthy = function Obj.Bool b -> b | _ -> true

  let evaluate_jump_not_truthy vm =
    let* b1 = Program_stack.read_then_increment vm.instructions in
    let* b2 = Program_stack.read_then_increment vm.instructions in
    let pos = ByteFmt.int_of_hex [b1; b2] 2 in
    let* condition = pop vm in
    if not (truthy condition) then vm.instructions.ip <- pos ;
    finish_run vm
end

(*FIXME any inperformant functions called in here is an issue *)
(*FIXME right now this function is a mess each sub function has to end in run {vm with instructions=rest} there is a better way*)
let[@ocaml.tailcall] [@ocaml.warning "-9-11"] run vm =
  let open Code in
  let match_opcode vm = function
    | `Constant _ | `CONSTANT ->
        VM_Helpers.evaluate_opconstant vm
    | (`Add | `Sub | `Mul | `Div) as op ->
        VM_Helpers.execute_binary_operation op vm
    | `True ->
        VM_Helpers.execute_bool true vm
    | `False ->
        VM_Helpers.execute_bool false vm
    | `Pop ->
        VM_Helpers.evaluate_oppop vm
    | (`Equal | `NotEqual | `GreaterThan) as op ->
        VM_Helpers.execute_comparison op vm
    | `Bang ->
        VM_Helpers.execute_bang vm
    | `Minus ->
        VM_Helpers.execute_minus vm
    (* NOTE Jumps will be difficult as I am dealing with an actual stack and not a list with a program counter *)
    | `Jump _ | `JUMP ->
        VM_Helpers.evaluate_jump vm
    | `JumpNotTruthy _ | `JUMPNOTTRUTHY ->
        VM_Helpers.evaluate_jump_not_truthy vm
    | a ->
        Error (Code.CodeError.OpCodeNotImplemented a)
  in
  Array.iter
    (fun a -> print_endline (Option.get a |> ByteFmt.pp_byte))
    vm.instructions.stack ;
  (* Perf issuse what happens here is the loop will continue even if the ip is done pointing at what it needs to  *)
  let x =
    Array.fold_left
      (fun vm _ ->
        let* vm = vm in
        match Program_stack.read_then_increment vm.instructions with
        | Ok hd ->
            let* {def} = lookup hd in
            match_opcode vm def
        | Error _ ->
            Ok vm )
      (Ok vm) vm.instructions.stack
  in
  x

(* match vm.instructions.stack with *)
(* | [| |] -> *)
(*     Ok vm *)
(* | instruction :: rest -> *)
(*     let* {def} = lookup instruction in *)
(*     let* vm = match_opcode rest def in *)
(*     run vm *)
