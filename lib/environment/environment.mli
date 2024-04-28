type environment = {store: (string, Object.item) Utils.Token_AssocList.t}

val get : environment -> string -> Object.item

val new_environment : unit -> environment

val set : environment -> string -> Object.item -> environment * Object.item
