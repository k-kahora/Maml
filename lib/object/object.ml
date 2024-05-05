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
    | Builtin of built_in_func

  and built_in_func = Obj.item list -> Obj.item

  val unwrap_error : item -> string

  val unwrap_return : item -> item

  val object_string : item -> string

  val item_to_string : item -> string

  val is_error : item -> bool

  val new_error : string -> item

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
    | Builtin of built_in_func

  and built_in_func = Obj.item list -> Obj.item

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
    | Builtin _ ->
        "BUILTIN"

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
    | Builtin _ ->
        "Builtin WIP"

  let is_return a = match a with Return _ -> true | _ -> false

  let is_error a = match a with Error _ -> true | _ -> false

  let new_error a = Error a
end

and Environment : sig
  type environment =
    { mutable store: (string, Obj.item) Utils.Token_AssocList.t
    ; mutable outer: environment option }

  val new_environment : unit -> environment

  val get : environment -> string -> Obj.item option

  val set : environment -> string -> Obj.item -> environment * Obj.item

  val new_enclosed_environment : environment -> environment
end = struct
  type environment =
    { mutable store: (string, Obj.item) Utils.Token_AssocList.t
    ; mutable outer: environment option }

  let new_environment () = {store= Utils.Token_AssocList.empty; outer= None}

  let new_enclosed_environment env =
    {store= Utils.Token_AssocList.empty; outer= Some env}

  let rec get env name =
    match Utils.Token_AssocList.find name env.store with
    | None ->
        (*Not in current env check outer binding*)
        Option.bind env.outer (fun a -> get a name)
    | Some i ->
        Some i

  let set env name value =
    let store = Utils.Token_AssocList.add name value env.store in
    ({env with store}, value)
end

and Builtin : sig
  val built_in_list : (string, Obj.item) Utils.Token_AssocList.t
end = struct
  let length args =
    match args with
    | [a] -> (
      match a with
      | Obj.String s ->
          Obj.Int (String.length s)
      | t ->
          Obj.new_error
          @@ Format.sprintf "argument to `len` not supported, got %s"
               (Obj.object_string t) )
    | _ ->
        Obj.new_error
        @@ Format.sprintf "wrong number of arguments. got=%d, want=1"
             (List.length args)

  let built_in_list =
    Utils.Token_AssocList.(empty |> add "len" (Obj.Builtin length))
end
