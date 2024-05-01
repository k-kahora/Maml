module rec Obj : sig
  type item =
    | Int of int
    | String of string
    | Bool of bool
    (* Parameters, Body, Environment *)
    | Function of Ast.expression list * Ast.statement * Environment.environment
    | Null
    | Return of item
    | Error of string

  val unwrap_error : item -> string

  val unwrap_return : item -> item

  val object_string : item -> string

  val item_to_string : item -> string

  val is_error : item -> bool

  val is_return : item -> bool
end = struct
  type item =
    | Int of int
    | String of string
    | Bool of bool
    | Function of Ast.expression list * Ast.statement * Environment.environment
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

  let object_string = function
    | Int _ ->
        "INTEGER"
    | String _ ->
        "STRING"
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
    | String i ->
        i
    | Bool b ->
        string_of_bool b
    | Function (_, _, _) ->
      "fn ("
    | Return it ->
        item_to_string it
    | Null ->
        "Null"
    | Error e ->
        "ERROR: " ^ e

  let is_return a = match a with Return _ -> true | _ -> false

  let is_error a = match a with Error _ -> true | _ -> false
end

and Environment : sig
  type environment = {mutable store: (string, Obj.item) Utils.Token_AssocList.t; mutable outer: environment option}

  val new_environment : unit -> environment

  val get : environment -> string -> Obj.item

  val set : environment -> string -> Obj.item -> environment * Obj.item

  val new_enclosed_environment: environment -> environment
end = struct
  type environment = {mutable store: (string, Obj.item) Utils.Token_AssocList.t; mutable outer: environment option}

  let new_environment () = {store= Utils.Token_AssocList.empty; outer=None}

  let new_enclosed_environment env = {store= Utils.Token_AssocList.empty; outer=Some env}

  let rec get env name =
    match Utils.Token_AssocList.find name env.store with
    | None -> (*Not in current env check outer binding*)
      (match env.outer with
        | None -> Obj.Error ("identifier not found: " ^ name)
        | Some e -> get e name)
    | Some i ->
      i

  let set env name value =
    let store = Utils.Token_AssocList.add name value env.store in
    ({env with store;}, value)
end
