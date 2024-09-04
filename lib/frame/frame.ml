open Object

type byte = char

type frame = {fn: byte Program_stack.program_stack; ip: int; base_pointer: int}

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

let inst ~default {fn; ip= _; base_pointer= _} =
  fn |> Program_stack.list_of_programstack ~default

let num_locals a = item_check a |> snd

let num_parameters a = item_check a |> thrd

let new_frame comp_func ~base_pointer =
  let lst = item_check comp_func |> fst |> Program_stack.stack_of_list in
  {fn= lst; ip= -1; base_pointer}

let new_default_frame () =
  new_frame
    (Obj.Closure {fn= Obj.CompFunc ([], 0, 0); free= []})
    ~base_pointer:0
