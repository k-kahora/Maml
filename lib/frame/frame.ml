open Object

type byte = char

type frame = {fn: byte Program_stack.program_stack; ip: int}

let item_check = function
  | Obj.CompFunc lst ->
      lst
  | e ->
      failwith
        (Format.sprintf
           "This should never trigger, something is horribly wrong: shold be \
            compfunct got -> %s"
           (Obj.item_to_string e) )

let new_frame comp_func =
  let lst = item_check comp_func |> Program_stack.stack_of_list in
  {fn= lst; ip= -1}
