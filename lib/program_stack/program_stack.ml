open Code

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

let increment ps =
  let x = ps.ip in
  ps.ip <- x + 1

let deincrement ps =
  let x = ps.ip in
  ps.ip <- x - 1

let push item ps =
  print_int ps.ip ;
  ps.stack.(ps.ip) <- Some item ;
  increment ps

let pop ps =
  if ps.ip <= 0 then Error CodeError.EmptyStack
  else (
    deincrement ps ;
    let poped = ps.stack.(ps.ip) in
    ps.stack.(ps.ip) <- None ;
    Ok poped )

let head ps = if ps.ip <= 0 then None else ps.stack.(ps.ip)
