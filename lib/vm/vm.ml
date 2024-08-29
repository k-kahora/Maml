open Object

let ( let* ) = Result.bind

(* FIXME set it to this once in prod *)
let stack_size = 2048

let global_size = 65536

let max_frames = 1024
(* all operands are 2 bytes wide or 16 bits so FFFF is the max  amount of globals allowed `SetGlobal 65536*)

type byte = char

module IntMap = Map.Make (Int)

type virtual_machine =
  { constants: Obj.item IntMap.t
  ; globals: Obj.item Program_stack.program_stack
  ; last_item_poped: Obj.item
  ; frames: Frame.frame array
  ; frame_index: int
  ; stack: Obj.item Program_stack.program_stack }

let constant_string vm =
  let str =
    IntMap.fold
      (fun idx value acc ->
        acc ^ Format.sprintf "%d: %s\n" idx (Obj.item_to_string value) )
      vm.constants ""
  in
  str

let globals_string vm =
  let str =
    Array.fold_left
      (fun acc item ->
        let value = Option.value ~default:Obj.Null item in
        acc ^ Format.sprintf "%s, " (Obj.item_to_string value) )
      "" vm.globals.stack
  in
  str

let string_of_vm vm =
  Format.sprintf "\nconstants: \n%s\nglobals: %s\n\n" (constant_string vm)
    (globals_string vm)

let last_item_popped vm =
  Program_stack.head vm.stack |> Result.value ~default:(Obj.String "Null Item!!")

let print_stack vm =
  Format.printf "Stack::Pointer::%d\n" vm.stack.ip ;
  Array.iteri
    (fun idx item ->
      Format.printf "%40d %s\n" idx
        (Option.fold ~none:"None" ~some:Obj.item_to_string item) )
    vm.stack.stack

let push_frame frame vm =
  vm.frames.(vm.frame_index + 1) <- frame ;
  {vm with frame_index= vm.frame_index + 1}

let current_frame vm = vm.frames.(vm.frame_index)

let current_instructions vm =
  Frame.inst ~default:'\x00' vm.frames.(vm.frame_index)

let pop_frame vm =
  let old_frame = current_frame vm in
  vm.frames.(vm.frame_index) <-
    Frame.new_frame (Obj.CompFunc ([], 0, 0)) ~base_pointer:0 ;
  let new_vm = {vm with frame_index= vm.frame_index - 1} in
  (new_vm, old_frame)

let update_current_frame frame vm = vm.frames.(vm.frame_index) <- frame

let frame_instructions vm = current_frame vm |> fun a -> a.fn

let new_virtual_machine byte_code =
  let open Compiler in
  let main_fn = Obj.CompFunc (current_instructions byte_code, 0, 0) in
  let main_frame = Frame.new_frame main_fn ~base_pointer:0 in
  let frames =
    Array.make max_frames
    @@ Frame.new_frame (Obj.CompFunc ([], 0, 0)) ~base_pointer:0
  in
  frames.(0) <- main_frame ;
  { constants= byte_code.constants
  ; globals= Program_stack.make_stack global_size
  ; frames
  ; frame_index= 0 (* ; last_item_poped= Obj.Null *)
  ; last_item_poped= Obj.String "test"
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

  let finish_run vm = (* print_stack vm ; *) Ok vm

  let evaluate_opconstant vm =
    let* b1 = Program_stack.read_then_increment (frame_instructions vm) in
    let* b2 = Program_stack.read_then_increment (frame_instructions vm) in
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
    let* _ = pop vm in
    finish_run vm
  (* {vm with last_item_poped= a} |> finish_run *)

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
    let* b1 = Program_stack.read_then_increment (frame_instructions vm) in
    let* b2 = Program_stack.read_then_increment (frame_instructions vm) in
    let jump_pos = ByteFmt.int_of_hex [b1; b2] 2 in
    (frame_instructions vm).ip <- jump_pos ;
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
    let* b1 = Program_stack.read_then_increment (frame_instructions vm) in
    let* b2 = Program_stack.read_then_increment (frame_instructions vm) in
    let pos = ByteFmt.int_of_hex [b1; b2] 2 in
    let* condition = pop vm in
    if not (truthy condition) then (frame_instructions vm).ip <- pos ;
    finish_run vm

  let evaluate_get_global vm =
    let* b1 = Program_stack.read_then_increment (frame_instructions vm) in
    let* b2 = Program_stack.read_then_increment (frame_instructions vm) in
    let global_index = ByteFmt.int_of_hex [b1; b2] 2 in
    let* item = Program_stack.get global_index vm.globals in
    push item vm ; finish_run vm

  let evaluate_set_global vm =
    let* b1 = Program_stack.read_then_increment (frame_instructions vm) in
    let* b2 = Program_stack.read_then_increment (frame_instructions vm) in
    let global_index = ByteFmt.int_of_hex [b1; b2] 2 in
    let* poped = pop vm in
    let* _ = Program_stack.set global_index poped vm.globals in
    finish_run vm

  let evaluate_get_local vm =
    let* b1 = Program_stack.read_then_increment (frame_instructions vm) in
    let* b2 = Program_stack.read_then_increment (frame_instructions vm) in
    let local_index = ByteFmt.int_of_hex [b1; b2] 2 in
    let frame = current_frame vm in
    (* update_current_frame {frame with ip= frame.ip + 1} vm ; *)
    let* item = Program_stack.get (frame.base_pointer + local_index) vm.stack in
    push item vm ; finish_run vm

  let evaluate_set_local vm =
    let* b1 = Program_stack.read_then_increment (frame_instructions vm) in
    let* b2 = Program_stack.read_then_increment (frame_instructions vm) in
    let local_index = ByteFmt.int_of_hex [b1; b2] 2 in
    let frame = current_frame vm in
    (* update_current_frame {frame with ip= frame.ip + 1} vm ; *)
    let* poped_item = pop vm in
    let* _ =
      Program_stack.set (frame.base_pointer + local_index) poped_item vm.stack
    in
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
      let return_item = Obj.Hash hash in
      Ok return_item
    in
    let* b1 = Program_stack.read_then_increment (frame_instructions vm) in
    let* b2 = Program_stack.read_then_increment (frame_instructions vm) in
    let num_elements = ByteFmt.int_of_hex [b1; b2] 2 in
    let* hash = build_hash (vm.stack.ip - num_elements) vm.stack.ip vm in
    (* NOTE this was a crucial step I forget, this could all be avoide with non imperative code *)
    vm.stack.ip <- vm.stack.ip - num_elements ;
    push hash vm ;
    finish_run vm

  let evaluate_array vm =
    let reverse_array arr =
      let len = Array.length arr in
      Array.init len (fun i -> arr.(len - i - 1))
    in
    let* b1 = Program_stack.read_then_increment (frame_instructions vm) in
    let* b2 = Program_stack.read_then_increment (frame_instructions vm) in
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

  let call_function comp_func num_args vm =
    let expected_args = Frame.num_parameters comp_func in
    let* vm =
      if num_args <> expected_args then
        Error (Code.CodeError.WrongNumberOfArguments (expected_args, num_args))
      else Ok vm
    in
    let frame =
      Frame.new_frame comp_func ~base_pointer:(vm.stack.ip - num_args)
    in
    let vm = push_frame frame vm in
    print_endline "func instructions" ;
    Code.string_of_byte_list (current_instructions vm)
    |> Result.value ~default:"Failed to generate bytecode"
    |> print_endline ;
    vm.stack.ip <- frame.base_pointer + Frame.num_locals comp_func ;
    Ok vm

  let call_builtin built_in num_args vm =
    let args = vm.stack.ip - num_args in
    let arg_list =
      List.init num_args (fun i -> Program_stack.get (i + args) vm.stack)
    in
    let _is_error =
      List.find_opt Result.is_error arg_list
      (* |> Option.to_result ~none:(List.map Result.get_ok) *)
    in
    let x = List.map Result.get_ok arg_list in
    let result =
      match built_in with
      | Obj.Builtin fn ->
          fn x
      | _ ->
          failwith "should never trigger"
    in
    vm.stack.ip <- vm.stack.ip - num_args - 1 ;
    (* if Result.is_ok result then push (Result.get_ok result) vm *)
    (* else push Obj.Null vm ; *)
    push result vm ;
    finish_run vm

  let execute_call num_args vm =
    let* calle = Program_stack.get (vm.stack.ip - 1 - num_args) vm.stack in
    match calle with
    | Obj.CompFunc _ ->
        call_function calle num_args vm
    | Obj.Builtin _ ->
        call_builtin calle num_args vm
    | _ ->
        Error
          (Code.CodeError.CustomError "calling non-function and non-built-in")

  (** [evaluate_call vm] get the current item on the top of the stack which should be a CompiledFunc 100% of the time, a exception is thrown if not; make a new frame with it and push it onto the stack_frames  *)
  let evaluate_call vm =
    let* b1 = Program_stack.read_then_increment (frame_instructions vm) in
    let num_of_args = ByteFmt.int_of_hex [b1] 1 in
    let* vm = execute_call num_of_args vm in
    finish_run vm

  let return_value vm =
    let* return_value = pop vm in
    let vm, frame = pop_frame vm in
    (* let* _ = pop vm in *)
    vm.stack.ip <- frame.base_pointer - 1 ;
    push return_value vm ;
    finish_run vm

  let return vm =
    let vm, frame = pop_frame vm in
    (* let* _ = pop vm in *)
    vm.stack.ip <- frame.base_pointer - 1 ;
    push Obj.Null vm ;
    finish_run vm

  let evaluate_builtin vm =
    let* b1 = Program_stack.read_then_increment (frame_instructions vm) in
    let built_in_index = ByteFmt.int_of_hex [b1] 1 in
    let* _, def =
      List.nth_opt Object.Builtin.builtins built_in_index
      |> Option.to_result
           ~none:
             (Code.CodeError.CustomError
                "Builtin index not found, maybe built in you want is not \
                 implemented" )
    in
    push def vm ; finish_run vm
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
    | `SetLocal _ | `SETLOCAL ->
        VM_Helpers.evaluate_set_local vm
    | `GetLocal _ | `GETLOCAL ->
        VM_Helpers.evaluate_get_local vm
    | `Array _ | `ARRAY ->
        VM_Helpers.evaluate_array vm
    | `Hash _ | `HASH ->
        VM_Helpers.evaluate_hash vm
    | `Call _ | `CALL ->
        VM_Helpers.evaluate_call vm
    | `ReturnValue ->
        VM_Helpers.return_value vm
    | `Return ->
        VM_Helpers.return vm
    | `GetBuiltIn _ | `GETBUILTIN ->
        VM_Helpers.evaluate_builtin vm
    (* | a -> *)
    (*     Error (Code.CodeError.OpCodeNotImplemented a) *)
  in
  (* Perf issuse what happens here is the loop will continue even if the ip is done pointing at what it needs to  *)
  (* let x = *)
  (*   Array.fold_left *)
  (*     (fun vm _ -> *)
  (*       let* vm = vm in *)
  (*       match Program_stack.read_then_increment (frame_instructions vm) with *)
  (*       | Ok hd -> *)
  (*           let* {def} = lookup hd in *)
  (*           match_opcode vm def *)
  (*       | Error _ -> *)
  (*           Ok vm ) *)
  (*     (Ok vm) (frame_instructions vm).stack *)
  (* in *)
  (* x *)
  let rec helper acc =
    match Program_stack.read_then_increment @@ frame_instructions acc with
    | Ok hd ->
        let* {def} = lookup hd in
        let* acc = match_opcode acc def in
        helper acc
    | Error _ ->
        Ok acc
  in
  helper vm

(* match vm.instructions.stack with *)
(* | [| |] -> *)
(*     Ok vm *)
(* | instruction :: rest -> *)
(*     let* {def} = lookup instruction in *)
(*     let* vm = match_opcode rest def in *)
(*     run vm *)
