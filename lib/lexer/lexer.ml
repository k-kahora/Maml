type lexer = {input: string; position: int; readPosition: int; ch: char}

let read_char (l : lexer) : lexer =
  { l with
    ch=
      ( if l.readPosition >= String.length l.input then '\x00'
        else String.get l.input l.readPosition )
  ; readPosition= l.readPosition + 1 }

let new' (input : string) : lexer =
  let l = {input; position= 0; readPosition= 0; ch= '\x00'} in
  read_char l

(* Give us the next char and advance the position *)
