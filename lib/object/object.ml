type 'a obj_ops = {to_str: 'a -> string; print: 'a -> unit}

type item = Int of int | Bool of bool | Null

let item_to_string = function
  | Int i ->
      string_of_int i
  | Bool b ->
      string_of_bool b
  | Null ->
      "Null"
