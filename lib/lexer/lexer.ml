type lexer = {input: string; position: int; readPosition: int; ch: char}

let read_char (l : lexer) : lexer =
  { l with
    ch=
      ( if l.readPosition >= String.length l.input then '\x00'
        else String.get l.input l.readPosition
          (* Set readRead position to the next character *) )
  ; readPosition= l.readPosition + 1
  ; position= l.readPosition }

(* Give us the next char and advance the position *)
let new' (input : string) : lexer =
  let l = {input; position= 0; readPosition= 0; ch= '\x00'} in
  read_char l

let newToken (token_type : Token.token_name) (ch : char) : Token.token =
  {literal= String.make 1 ch; type'= token_type}

(* given a lexer returen the correct token and advance the lexer *)
let next_token (l : lexer) : lexer * Token.token =
  let token =
    match l.ch with
    | '=' ->
        newToken Token.ASSIGN l.ch
    | ';' ->
        newToken Token.SEMICOLON l.ch
    | '(' ->
        newToken Token.LPAREN l.ch
    | ')' ->
        newToken Token.RPAREN l.ch
    | ',' ->
        newToken Token.COMMA l.ch
    | '+' ->
        newToken Token.PLUS l.ch
    | '{' ->
        newToken Token.LBRACE l.ch
    | '}' ->
        newToken Token.RBRACE l.ch
    | _ ->
        newToken Token.EOF l.ch
  in
  let l = read_char l in
  (l, token)
