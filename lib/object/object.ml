type item =
  | Int of int
  | Bool of bool
  | Function of Ast.ident list * Ast.expression (* * Environment.environment *)
  | Null
  | Return of item
  | Error of string

let unwrap_error = function
  | Error a ->
      a
  | _ ->
      failwith "unwrap error can only unwrap a return object"

let unwrap_return = function
  | Return a ->
      a
  | _ ->
      failwith "unwrap return can only unwrap a return object"

let object_string obj =
  match obj with
  | Int _ ->
      "INTEGER"
  | Bool _ ->
      "BOOLEAN"
  | Function _ ->
      "FUNCTION"
  | Null ->
      "Null"
  | Return _ ->
      "RETURN"
  | Error _ ->
      "ERROR"

let rec item_to_string = function
  | Int i ->
      string_of_int i
  | Bool b ->
      string_of_bool b
  | Return it ->
      item_to_string it
  | Null ->
      "Null"
  | Error e ->
      "ERROR: " ^ e

let is_return a = match a with Return _ -> true | _ -> false

let is_error a = match a with Error _ -> true | _ -> false
