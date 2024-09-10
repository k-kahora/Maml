let ( let* ) = Result.bind

module rec Obj : sig
  type item_type =
    | Int'
    | String'
    | Bool'
    (* Parameters, Body, Environment *)
    | Function'
    | CompFunc'
    | Null'
    | Return'
    | Error'
    | Array'
    | Builtin'
    | HashKey'
    | Hash'
    | Closure'

  and hash_item = {key: item; value: item}

  and item =
    | Int of int
    | String of string
    | Bool of bool
    (* Parameters, Body, Environment *)
    | Function of Ast.expression list * Ast.statement * Environment.environment
    (* bytecode followed by num of locals in the function *)
    (* The first number is the number of local vars and the next is num of parameters *)
    | CompFunc of char list * int * int
    | Null
    | Return of item
    | Error of string
    | Array of item array
    | Builtin of built_in_func
    | HashKey of item_type * int64
    | Hash of (Obj.item, hash_item) Hashtbl.t
    (* Fn object should always be a CompFunc *)
    | Closure of {fn: Obj.item; free: Obj.item list}

  and built_in_func = Obj.item list -> Obj.item

  val hashable : item -> bool

  val unwrap_error : item -> string

  val unwrap_return : item -> item

  val object_string : item -> string

  val item_to_string : item -> string

  val is_error : item -> bool

  val new_error : string -> item

  (* Maybe try caching the return value to increase preformance so we are not always calling the xx lib *)
  val hash_key : Obj.item -> Obj.item

  val is_return : item -> bool
end = struct
  type item_type =
    | Int'
    | String'
    | Bool'
    (* Parameters, Body, Environment *)
    | Function'
    | CompFunc'
    | Null'
    | Return'
    | Error'
    | Array'
    | Builtin'
    | HashKey'
    | Hash'
    | Closure'

  and hash_item = {key: item; value: item}

  and item =
    | Int of int
    | String of string
    | Bool of bool
    | Function of Ast.expression list * Ast.statement * Environment.environment
    | CompFunc of char list * int * int
    | Null
    | Return of item
    | Error of string
    | Array of item array
    | Builtin of built_in_func
    | HashKey of item_type * int64
    | Hash of (Obj.item, hash_item) Hashtbl.t
    | Closure of {fn: Obj.item; free: Obj.item list}

  and built_in_func = Obj.item list -> Obj.item

  let item_of_item_type = function
    | Int _ ->
        Int'
    | String _ ->
        String'
    | Bool _ ->
        Bool'
    | Function _ ->
        Function'
    | CompFunc _ ->
        CompFunc'
    | Null ->
        Null'
    | Return _ ->
        Return'
    | Error _ ->
        Error'
    | Array _ ->
        Array'
    | Builtin _ ->
        Builtin'
    | HashKey _ ->
        HashKey'
    | Closure _ ->
        Closure'
    | Hash _ ->
        Hash'

  let make_hash item_type hash = HashKey (item_of_item_type item_type, hash)

  (* Make this return a result *)
  let hash_key = function
    | Int a as item_t ->
        make_hash item_t @@ Int64.of_int a
    | String a as item_t ->
        make_hash item_t @@ XXHash.XXH64.hash a
    | Bool a as item_t ->
        make_hash item_t @@ if a then 1L else 0L
    | Function (_a, _b, _c) ->
        failwith "Function not yet implemented"
    | Null ->
        failwith "Null not yet implemented"
    | Return _a ->
        failwith "Return not yet implemented"
    | Error _a ->
        failwith "Error not yet implemented"
    | Array _a ->
        failwith "Array not yet implemented"
    | Builtin _a ->
        failwith "Builtin not yet implemented"
    | HashKey (_a, _b) ->
        failwith "HashKey not yet implemented"
    | Hash _ ->
        failwith "Hash not yet implemented"
    | CompFunc _ ->
        failwith "CompFunc not yet implemented"
    | Closure _ ->
        failwith "Clojure not yet implemented"

  let hashable = function
    | Int _ ->
        true
    | Bool _ ->
        true
    | String _ ->
        true
    | Null ->
        false
    | Return _ ->
        false
    | Error _ ->
        false
    | Array _ ->
        false
    | Builtin _ ->
        false
    | HashKey _ ->
        false
    | Function _ ->
        false
    | CompFunc _ ->
        false
    | Hash _ ->
        false
    | Closure _ ->
        false

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
    | Array _ ->
        "ARRAY"
    | Int _ ->
        "INTEGER"
    | String _ ->
        "STRING"
    | Bool _ ->
        "BOOLEAN"
    | Function _ ->
        "FUNCTION"
    | CompFunc _ ->
        "CompFunc"
    | Null ->
        "Null"
    | Return _ ->
        "RETURN"
    | Error _ ->
        "ERROR"
    | Builtin _ ->
        "BUILTIN"
    | HashKey _ ->
        "HASHKEY"
    | Hash _ ->
        "HASH"
    | Closure _ ->
        "Closure"

  let rec item_to_string = function
    | Int i ->
        string_of_int i
    | String i ->
        i
    | Bool b ->
        string_of_bool b
    (* | CompFunc (_ls, _locals) -> *)
    (*     "COMPFUNC" *)
    | CompFunc (ls, _locals, _params) ->
        List.fold_left
          (fun acc a -> acc ^ Format.sprintf "0x%02X," (int_of_char a))
          "[" ls
        ^ "]"
    | Function (parmeters, _, _) ->
        let str =
          List.fold_left
            (fun acc nxt -> acc ^ Format.sprintf "%s, " (Ast.expr_str nxt))
            "fn (" parmeters
        in
        str ^ ")"
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
    | HashKey (_, v) ->
        Int64.to_string v
    | Hash table ->
        Hashtbl.fold
          (fun _hash_code {key= true_key; value} acc ->
            acc
            ^ Format.sprintf "%s:%s, " (item_to_string true_key)
                (item_to_string value) )
          table "{"
        ^ "}"
    | Closure _ ->
        "Closure string not yet implemented"

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

  val builtins : (string * Obj.item) list
end = struct
  let pp_err = Format.sprintf

  let make_error str = Obj.Error str

  let puts args =
    let puts_string =
      List.fold_left (fun acc nxt -> acc ^ Obj.item_to_string nxt) "" args
    in
    print_endline puts_string ; (* Ok (Obj.String puts_string) *) Obj.Null

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
          make_error
            (pp_err "argument to `push` must be ARRAY, got %s"
               (Obj.object_string t) ) )
    | _ ->
        make_error
          (pp_err "wrong number of arguments. got=%d, want=1" (List.length args))

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
          make_error
            (pp_err "argument to `rest` must be ARRAY, got %s"
               (Obj.object_string t) ) )
    | _ ->
        make_error
          (pp_err "wrong number of arguments. got=%d, want=1" (List.length args))

  let last args =
    match args with
    | [a] -> (
      match a with
      | Obj.Array arr ->
          if Array.length arr > 1 then arr.(Array.length arr - 1) else Obj.Null
      | t ->
          make_error
            (pp_err "argument to `last` must be ARRAY, got %s"
               (Obj.object_string t) ) )
    | _ ->
        make_error
          (pp_err "wrong number of arguments. got=%d, want=1" (List.length args))

  let first args =
    match args with
    | [a] -> (
      match a with
      | Obj.Array arr ->
          if Array.length arr > 0 then arr.(0) else Obj.Null
      | t ->
          make_error
            (pp_err "argument to `first` must be ARRAY, got %s"
               (Obj.object_string t) ) )
    | _ ->
        make_error
          (pp_err "wrong number of arguments. got=%d, want=1" (List.length args))

  let length args =
    match args with
    | [a] -> (
      match a with
      | Obj.String s ->
          Obj.Int (String.length s)
      | Obj.Array arr ->
          Obj.Int (Array.length arr)
      | t ->
          make_error
            (pp_err "argument to `len` not supported, got %s"
               (Obj.object_string t) ) )
    | _ ->
        make_error
          (pp_err "wrong number of arguments. got=%d, want=1" (List.length args))

  (* This is easier to iterate over than a map *)
  let builtins =
    [ ("len", Obj.Builtin length)
    ; ("puts", Obj.Builtin puts)
    ; ("first", Obj.Builtin first)
    ; ("last", Obj.Builtin last)
    ; ("rest", Obj.Builtin rest)
    ; ("push", Obj.Builtin push) ]

  let get_built_in_by_name name =
    let built_in = List.find (fun (a, _) -> name = a) builtins in
    built_in |> snd

  let built_in_list =
    Utils.Token_AssocList.(
      empty
      |> add "len" (get_built_in_by_name "len")
      |> add "first" (get_built_in_by_name "first")
      |> add "last" (get_built_in_by_name "last")
      |> add "push" (get_built_in_by_name "push")
      |> add "puts" (get_built_in_by_name "puts")
      |> add "rest" (get_built_in_by_name "rest") )
end
