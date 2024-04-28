type environment = {store: (string, Object.item) Utils.Token_AssocList.t}

let new_environment () = {store= Utils.Token_AssocList.empty}

let get env name =
  Option.value ~default:Object.Null (Utils.Token_AssocList.find name env.store)

let set env name value =
  let store = Utils.Token_AssocList.add name value env.store in
  ({store}, value)
