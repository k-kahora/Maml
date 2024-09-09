open Object

type byte = char

type frame =
  { cl: Obj.item
  ; ip: int
  ; base_pointer: int
  ; internal_stack: byte Program_stack.program_stack }

let item_check = function
  | Obj.Closure {fn; free= _} -> (
    match fn with
    | Obj.CompFunc (lst, num_locals, parameters) ->
        (lst, num_locals, parameters)
    | e ->
        (* Replace with a proper error *)
        failwith
          (Format.sprintf
             "This should never trigger, something is horribly wrong: shold be \
              compfunct got -> %s, calling a non function"
             (Obj.item_to_string e) ) )
  | e ->
      (* Replace with a proper error *)
      failwith
        (Format.sprintf
           "This should never trigger wigger, something is horribly wrong: \
            shold be compfunct got -> %s, calling a non function"
           (Obj.item_to_string e) )

let thrd (_, _, c) = c

let snd (_, b, _) = b

let fst (a, _, _) = a

let inst ~default {cl= _; ip= _; base_pointer= _; internal_stack} =
  internal_stack |> Program_stack.list_of_programstack ~default

let num_locals a = item_check a |> snd

let num_parameters a = item_check a |> thrd

let new_frame closure ~base_pointer =
  let stack = item_check closure |> fst |> Program_stack.stack_of_list in
  let frame = {cl= closure; ip= -1; base_pointer; internal_stack= stack} in
  frame

let new_default_frame () =
  new_frame
    (Obj.Closure {fn= Obj.CompFunc ([], 0, 0); free= []})
    ~base_pointer:0
