open Object

let ( let* ) = Result.bind

let stack_size = 2048

let global_size = 65536
(* all operands are 2 bytes wide or 16 bits so FFFF is the max  amount of globals allowed `SetGlobal 65536*)

type byte = char

module IntMap = Map.Make (Int)

type virtual_machine =
  { constants: Obj.item IntMap.t
  ; instructions: byte Program_stack.program_stack
  ; globals: Obj.item Program_stack.program_stack
  ; last_item_poped: Obj.item
  ; stack: Obj.item Program_stack.program_stack }

let new_virtual_machine byte_code =
  let open Compiler in
  { constants= byte_code.constants
  ; instructions=
      Program_stack.stack_of_list (Compiler.current_instructions byte_code)
  ; globals= Program_stack.make_stack global_size
  ; last_item_poped= Obj.Null
  ; stack= Program_stack.make_stack stack_size }

let empty_globals () = Program_stack.make_stack global_size

let new_with_global_store compiler globals =
  let vm = new_virtual_machine compiler in
  {vm with globals}

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
    let execute_binary_string_operation left right = function
      | `Add ->
          Ok (left ^ right)
      | a ->
          Error
            (CodeError.UnsuportedOperator
               (Obj.String "dummy", Code.infix_operand_string a) )
    in
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
    | Obj.String l, Obj.String r ->
        let* result = execute_binary_string_operation l r op in
        push (Obj.String result) vm ;
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
      | Obj.Int i ->
          Ok (Obj.Bool (i == 0))
      | Obj.Bool b ->
          Ok (Obj.Bool (not b))
      | Obj.Null ->
          Ok (Obj.Bool true)
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

  let truthy = function
    | Obj.Bool b ->
        b
    | Obj.Int i ->
        i <> 0
    | Obj.Null ->
        false
    | _ ->
        true

  let evaluate_jump_not_truthy vm =
    let* b1 = Program_stack.read_then_increment vm.instructions in
    let* b2 = Program_stack.read_then_increment vm.instructions in
    let pos = ByteFmt.int_of_hex [b1; b2] 2 in
    let* condition = pop vm in
    if not (truthy condition) then vm.instructions.ip <- pos ;
    finish_run vm

  let evaluate_get_global vm =
    let* b1 = Program_stack.read_then_increment vm.instructions in
    let* b2 = Program_stack.read_then_increment vm.instructions in
    let global_index = ByteFmt.int_of_hex [b1; b2] 2 in
    let* item = Program_stack.get global_index vm.globals in
    push item vm ; finish_run vm

  let evaluate_set_global vm =
    let* b1 = Program_stack.read_then_increment vm.instructions in
    let* b2 = Program_stack.read_then_increment vm.instructions in
    let global_index = ByteFmt.int_of_hex [b1; b2] 2 in
    let* poped = pop vm in
    let* _ = Program_stack.set global_index poped vm.globals in
    finish_run vm

  let evaluate_hash vm =
    let build_hash start_index end_index vm =
      let slice =
        Array.sub vm.stack.stack start_index (end_index - start_index)
        |> Array.to_list
      in
      let rec looper acc arr =
        match arr with
        | [] ->
            acc
        | [Some _]
        | [None]
        | None :: None :: _
        | None :: _ :: _
        | _ :: None :: _ ->
            Error
              (CodeError.CustomError
                 "Hash shold have even num of keys and values" )
        | Some key :: Some value :: rest ->
            let* hash_tbl = acc in
            let item = {Obj.key; value} in
            Hashtbl.add hash_tbl (Obj.hash_key key) item ;
            looper (Ok hash_tbl) rest
      in
      let* hash = looper (Ok (Hashtbl.create (List.length slice))) slice in
      Ok (Obj.Hash hash)
    in
    let* b1 = Program_stack.read_then_increment vm.instructions in
    let* b2 = Program_stack.read_then_increment vm.instructions in
    let num_elements = ByteFmt.int_of_hex [b1; b2] 2 in
    let* hash = build_hash (vm.stack.ip - num_elements) vm.stack.ip vm in
    push hash vm ; finish_run vm

  let evaluate_array vm =
    let reverse_array arr =
      let len = Array.length arr in
      Array.init len (fun i -> arr.(len - i - 1))
    in
    let* b1 = Program_stack.read_then_increment vm.instructions in
    let* b2 = Program_stack.read_then_increment vm.instructions in
    let arr_length = ByteFmt.int_of_hex [b1; b2] 2 in
    let t_arr = Array.init arr_length (fun _ -> pop vm) in
    let error = Array.find_opt Result.is_error t_arr in
    (* get_ok will never error because we checked for errors above *)
    let* item_array =
      match error with
      | None ->
          Ok (Array.map Result.get_ok t_arr)
      | Some err ->
          Error (Result.get_error err)
    in
    let item_array = reverse_array item_array in
    push (Obj.Array item_array) vm ;
    finish_run vm

  let evaluate_index vm =
    let execute_vm_expression left index vm =
      match (left, index) with
      | Obj.Array array, Obj.Int index ->
          if index >= 0 && index < Array.length array then
            Ok (push array.(index) vm)
          else Ok (push Obj.Null vm)
      | Obj.Hash hash, _ ->
          if not (Obj.hashable index) then
            Error (CodeError.CustomError "Index is not hashable")
          else
            let item =
              Hashtbl.find_opt hash (Obj.hash_key index)
              |> Option.fold ~none:Obj.Null ~some:(fun a -> a.Obj.value)
            in
            push item vm ; Ok ()
      (* FIXME make a better error here *)
      | _ ->
          Error (CodeError.CustomError "Index operator not supported")
    in
    let* index = pop vm in
    let* left = pop vm in
    let* _ = execute_vm_expression left index vm in
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
    | `Index ->
        VM_Helpers.evaluate_index vm
    (* NOTE Jumps will be difficult as I am dealing with an actual stack and not a list with a program counter *)
    | `Jump _ | `JUMP ->
        VM_Helpers.evaluate_jump vm
    | `JumpNotTruthy _ | `JUMPNOTTRUTHY ->
        VM_Helpers.evaluate_jump_not_truthy vm
    | `Null ->
        push Obj.Null vm ; VM_Helpers.finish_run vm
    | `SetGlobal _ | `SETGLOBAL ->
        VM_Helpers.evaluate_set_global vm
    | `GetGlobal _ | `GETGLOBAL ->
        VM_Helpers.evaluate_get_global vm
    | `Array _ | `ARRAY ->
        VM_Helpers.evaluate_array vm
    | `Hash _ | `HASH ->
        VM_Helpers.evaluate_hash vm
    | a ->
        Error (Code.CodeError.OpCodeNotImplemented a)
  in
  (* Array.iter *)
  (*   (fun a -> print_endline (Option.get a |> ByteFmt.pp_byte)) *)
  (*   vm.instructions.stack ; *)
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
