type 'a obj_ops = {to_str: 'a -> string; print: 'a -> unit}

type item = Int of int | Bool of bool | Null | Return of item

let unwrap_return = function
  | Return a ->
      a
  | _ ->
      failwith "unwrap return can only unwrap a return object"

let rec item_to_string = function
  | Int i ->
      string_of_int i
  | Bool b ->
      string_of_bool b
  | Return it ->
      item_to_string it
  | Null ->
      "Null"

let is_return a = match a with Return _ -> true | _ -> false
