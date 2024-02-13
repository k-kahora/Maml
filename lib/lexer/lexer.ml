type lexer = {input: string; position: int; readPosition: int; ch: char}

let read_char (l : lexer) : lexer =
  { l with
    ch=
      ( if l.readPosition >= String.length l.input then '\x00'
        else String.get l.input l.readPosition
          (* Set readRead position to the next character *) )
  ; readPosition= l.readPosition + 1
  ; position= l.readPosition }

let new' (input : string) : lexer =
  let l = {input; position= 0; readPosition= 0; ch= '\x00'} in
  read_char l

(* Give us the next char and advance the position *)

let next_token (l : lexer) : Token.token =
  match l.ch with
  | '=' ->
      {Token.literal= l.ch; Token.type'= Token.ASSIGN}
  | ';' ->
      {Token.literal= l.ch; Token.type'= Token.SEMICOLON}
  | '(' ->
      {Token.literal= l.ch; Token.type'= Token.LPAREN}
  | ')' ->
      {Token.literal= l.ch; Token.type'= Token.RPAREN}
  | ',' ->
      {Token.literal= l.ch; Token.type'= Token.COMMA}
  | '+' ->
      {Token.literal= l.ch; Token.type'= Token.PLUS}
  | '{' ->
      {Token.literal= l.ch; Token.type'= Token.LBRACE}
  | '}' ->
      {Token.literal= l.ch; Token.type'= Token.RBRACE}
  | _ ->
      {Token.literal= '\x00'; Token.type'= Token.EOF}
