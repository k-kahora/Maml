open Code

let ( let* ) = Result.bind

type 'a program_stack = {mutable ip: int; stack: 'a option array}

let program_stack_eq stack1 stack2 =
  let a = stack1.ip = stack2.ip in
  let arr1, arr2 = (stack1.stack, stack2.stack) in
  let intermiated = Array.map2 (fun s1 s2 -> compare s1 s2) arr1 arr2 in
  let b = Array.for_all (fun a -> a = 0) intermiated in
  a && b

let pp_program_stack fmt ps =
  Format.fprintf fmt "{ip: %d; stack=[%s]}" ps.ip
    (Array.fold_left
       (fun acc nxt -> acc ^ Option.fold ~none:"None" ~some:string_of_int nxt)
       "" ps.stack )

let alc_program_stack = Alcotest.testable pp_program_stack program_stack_eq

let make_stack size = {ip= 0; stack= Array.make size None}

let check_bounds index ps =
  if index < Array.length ps.stack && index >= 0 then Ok ()
  else Error (Code.CodeError.CustomError "index out of bounds")

let stack_of_list list =
  let arr = Array.of_list list |> Array.map (fun a -> Some a) in
  {ip= 0; stack= arr}

let stack_head ps =
  let* _ = check_bounds (ps.ip - 1) ps in
  ps.stack.(ps.ip - 1)
  |> Option.to_result ~none:(Code.CodeError.CustomError "stack head empty item")

let head ps =
  let* _ = check_bounds ps.ip ps in
  ps.stack.(ps.ip)
  |> Option.to_result ~none:(Code.CodeError.CustomError "empty item")

let read_then_increment ps =
  let hd = head ps in
  ps.ip <- ps.ip + 1 ;
  hd

let increment ps =
  let x = ps.ip in
  ps.ip <- x + 1

let deincrement ps =
  let x = ps.ip in
  ps.ip <- x - 1

let push item ps =
  ps.stack.(ps.ip) <- Some item ;
  increment ps

let get index ps =
  let* _ = check_bounds index ps in
  ps.stack.(index)
  |> Option.to_result ~none:(Code.CodeError.CustomError "empty item")

let set index item ps =
  let* _ = check_bounds index ps in
  Ok (ps.stack.(index) <- Some item)

let pop ps =
  if ps.ip <= 0 then Error CodeError.EmptyStack
  else (
    deincrement ps ;
    let poped = ps.stack.(ps.ip) in
    ps.stack.(ps.ip) <- None ;
    Ok poped )
