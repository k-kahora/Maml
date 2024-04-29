type environment = {mutable store: (string, Object.item) Utils.Token_AssocList.t}

let new_environment () = {store= Utils.Token_AssocList.empty}

let get env name =
  match Utils.Token_AssocList.find name env.store with
  | None ->
      Object.Error ("identifier not found: " ^ name)
  | Some i ->
      i

let set env name value =
  let store = Utils.Token_AssocList.add name value env.store in
  ({store}, value)
