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
    | Array of item array
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
    | Array of item array
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
        "Builtin"
    | Array arr ->
        Format.sprintf "[%s]"
          ( Array.fold_left (fun acc next -> acc @ [item_to_string next]) [] arr
          |> String.concat ", " )

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
  let puts args =
    List.iter (fun a -> Format.printf "%s" (Obj.item_to_string a)) args ;
    Obj.Null

  let push args =
    match args with
    | [a; b] -> (
      match a with
      | Obj.Array arr ->
          let array_length = Array.length arr in
          if array_length < 1 then Obj.Array [|b|]
          else
            let arr_copy = Array.make (array_length + 1) Obj.Null in
            Array.blit arr 0 arr_copy 0 array_length ;
            arr_copy.(array_length) <- b ;
            (* Modifeis array in place *)
            Obj.Array arr_copy
      | t ->
          Obj.new_error
          @@ Format.sprintf "argument to `push` must be ARRAY, got %s"
               (Obj.object_string t) )
    | _ ->
        Obj.new_error
        @@ Format.sprintf "wrong number of arguments. got=%d, want=1"
             (List.length args)

  let rest args =
    match args with
    | [a] -> (
      match a with
      | Obj.Array arr ->
          if Array.length arr > 1 then
            let arr_copy = Array.copy arr in
            Obj.Array (Array.sub arr_copy 1 (Array.length arr_copy - 1))
          else if Array.length arr = 1 then Obj.Array [||]
          else Obj.Null
      | t ->
          Obj.new_error
          @@ Format.sprintf "argument to `rest` must be ARRAY, got %s"
               (Obj.object_string t) )
    | _ ->
        Obj.new_error
        @@ Format.sprintf "wrong number of arguments. got=%d, want=1"
             (List.length args)

  let last args =
    match args with
    | [a] -> (
      match a with
      | Obj.Array arr ->
          if Array.length arr > 1 then arr.(Array.length arr - 1) else Obj.Null
      | t ->
          Obj.new_error
          @@ Format.sprintf "argument to `last` must be ARRAY, got %s"
               (Obj.object_string t) )
    | _ ->
        Obj.new_error
        @@ Format.sprintf "wrong number of arguments. got=%d, want=1"
             (List.length args)

  let first args =
    match args with
    | [a] -> (
      match a with
      | Obj.Array arr ->
          if Array.length arr > 0 then arr.(0) else Obj.Null
      | t ->
          Obj.new_error
          @@ Format.sprintf "argument to `first` must be ARRAY, got %s"
               (Obj.object_string t) )
    | _ ->
        Obj.new_error
        @@ Format.sprintf "wrong number of arguments. got=%d, want=1"
             (List.length args)

  let length args =
    match args with
    | [a] -> (
      match a with
      | Obj.String s ->
          Obj.Int (String.length s)
      | Obj.Array arr ->
          Obj.Int (Array.length arr)
      | t ->
          Obj.new_error
          @@ Format.sprintf "argument to `len` not supported, got %s"
               (Obj.object_string t) )
    | _ ->
        Obj.new_error
        @@ Format.sprintf "wrong number of arguments. got=%d, want=1"
             (List.length args)

  let built_in_list =
    Utils.Token_AssocList.(
      empty
      |> add "len" (Obj.Builtin length)
      |> add "first" (Obj.Builtin first)
      |> add "last" (Obj.Builtin last)
      |> add "push" (Obj.Builtin push)
      |> add "puts" (Obj.Builtin puts)
      |> add "rest" (Obj.Builtin rest) )
end
