open Object

type built_in_func = Obj.item list -> Obj.item

and built_in = {fn: built_in_func}

let built_in_list =
  Utils.Token_AssocList.(
    empty
    |> add "len" (fun args ->
           match args with
           | [a] -> (
             match a with
             | Obj.String s ->
                 Obj.Int (String.length s)
             | t ->
                 Obj.new_error
                 @@ Format.sprintf "argument to 'len' not supported got %s"
                      (Obj.item_to_string t) )
           | _ ->
               Obj.new_error
               @@ Format.sprintf "wrong number of arguments expected 1 got=%d"
                    (List.length args) ) )
