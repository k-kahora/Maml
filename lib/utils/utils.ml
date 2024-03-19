let text = 10

module type AssocList = sig
  type ('a, 'b) t

  val empty : ('a, 'b) t

  val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

  val find : 'a -> ('a, 'b) t -> 'b option
end

module Token_AssocList : AssocList = struct
  type ('a, 'b) t = ('a * 'b) list

  let empty = []

  let add key value list = (key, value) :: list

  let rec find key list =
    match list with
    | (tok, fn) :: _ when tok = key ->
        Some fn
    | _ :: t ->
        find key t
    | [] ->
        None

  (* let mem key list = List.exists (fun (a, _) -> a = key) list *)
end
