open Object

type built_in_func = Obj.item list -> Obj.item

and built_in = {fn: built_in_func}

val built_in_list : (string, built_in_func) Utils.Token_AssocList.t
