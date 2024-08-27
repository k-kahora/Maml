open Object

type byte = char

type frame = {fn: byte Program_stack.program_stack; ip: int; base_pointer: int}

let item_check = function
  | Obj.CompFunc (lst, num_locals) ->
      (lst, num_locals)
  | e ->
      failwith
        (Format.sprintf
           "This should never trigger, something is horribly wrong: shold be \
            compfunct got -> %s"
           (Obj.item_to_string e) )

let num_locals a = item_check a |> snd

let new_frame comp_func ~base_pointer =
  let lst = item_check comp_func |> fst |> Program_stack.stack_of_list in
  {fn= lst; ip= -1; base_pointer}
