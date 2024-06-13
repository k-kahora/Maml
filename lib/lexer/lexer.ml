type lexer = {input: string; position: int; readPosition: int; ch: char}

let peek_char (l : lexer) : char =
  match l.ch with
  | _ when l.readPosition >= String.length l.input ->
      ' '
  | _ ->
      l.input.[l.readPosition]

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

let is_digit = function '0' .. '9' -> true | _ -> false

let read_number (l : lexer) : string * lexer =
  let rec looper i lex =
    if i >= String.length l.input || not (is_digit l.input.[i]) then (i, lex)
    else looper (i + 1) @@ read_char lex
  in
  let finish, lex = looper l.position l in
  (String.sub l.input l.position (finish - l.position), lex)

let is_letter (ch : char) : bool =
  match ch with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

let eat_whitespace (l : lexer) : lexer =
  let rec looper lex =
    match lex.ch with
    | ' ' | '\t' | '\n' | '\r' ->
        looper (read_char lex)
    | _ ->
        lex
  in
  looper l

let rec eat_comment (l : lexer) : lexer =
  (* Eat all space up until a new line *)
  match l.ch with '\n' | '\x00' -> l | _ -> eat_comment @@ read_char l

(* start at the lexers position *)
(* loop through until non-char *)
(* substring from the start to the length of the substring *)
(* length is equal to the finisting index - starting index *)
let read_identifier (l : lexer) : string * lexer =
  (* loop over the string until we come across a non-letter *)
  let start = l.position in
  let rec looper i lex =
    if i >= String.length l.input || not (is_letter l.input.[i]) then (i, lex)
    else looper (i + 1) @@ read_char lex
  in
  let finish, lex = looper start l in
  (String.sub l.input start (finish - start), lex)

(* given a lexer returen the correct token and advance the lexer *)
let rec next_token (l : lexer) : Token.token * lexer =
  let token l =
    match l.ch with
    | '"' ->
        let read_string l =
          let start = l.position in
          let rec aux l =
            match l.ch with
            | '"' | '\x00' ->
                (l.position - start, read_char l)
            | _ ->
                aux (read_char l)
          in
          let end_pos, l = aux l in
          (String.sub l.input start end_pos, l)
        in
        let str, l = read_string (read_char l) in
        ({Token.type'= Token.STRING; Token.literal= str}, l)
    | '=' -> (
      match peek_char l with
      | '=' ->
          ( { Token.literal= String.make 1 l.ch ^ String.make 1 (peek_char l)
            ; Token.type'= Token.EQ }
          , read_char @@ read_char l )
      | _ ->
          (newToken Token.ASSIGN l.ch, read_char l) )
    | '+' ->
        (newToken Token.PLUS l.ch, read_char l)
    | '-' ->
        (newToken Token.MINUS l.ch, read_char l)
    | '!' -> (
      match peek_char l with
      | '=' ->
          ( { Token.type'= Token.NOT_EQ
            ; Token.literal= String.make 1 l.ch ^ String.make 1 (peek_char l) }
          , read_char @@ read_char l )
      | _ ->
          (newToken Token.BANG l.ch, read_char l) )
    | '/' -> (
      match peek_char l with
      | '/' ->
          next_token @@ eat_comment l
      | _ ->
          (newToken Token.SLASH l.ch, read_char l) )
    | '*' ->
        (newToken Token.ASTERISK l.ch, read_char l)
    | '<' -> (
      match peek_char l with
      | '=' ->
          ( { Token.type'= Token.LTEQ
            ; Token.literal= String.make 1 l.ch ^ String.make 1 (peek_char l) }
          , read_char @@ read_char l )
      | _ ->
          (newToken Token.LT l.ch, read_char l) )
    | '>' -> (
      match peek_char l with
      | '=' ->
          ( { Token.type'= Token.GTEQ
            ; Token.literal= String.make 1 l.ch ^ String.make 1 (peek_char l) }
          , read_char @@ read_char l )
      | _ ->
          (newToken Token.GT l.ch, read_char l) )
    | ';' ->
        (newToken Token.SEMICOLON l.ch, read_char l)
    | ':' ->
        (newToken Token.COLON l.ch, read_char l)
    | '(' ->
        (newToken Token.LPAREN l.ch, read_char l)
    | ')' ->
        (newToken Token.RPAREN l.ch, read_char l)
    | ',' ->
        (newToken Token.COMMA l.ch, read_char l)
    | '{' ->
        (newToken Token.LBRACE l.ch, read_char l)
    | '}' ->
        (newToken Token.RBRACE l.ch, read_char l)
    | '[' ->
        (newToken Token.LBRACKET l.ch, read_char l)
    | ']' ->
        (newToken Token.RBRACKET l.ch, read_char l)
    | '\x00' ->
        (newToken Token.EOF l.ch, read_char l)
    | _ when is_digit l.ch ->
        let number, l = read_number l in
        ({type'= Token.INT; literal= number}, l)
    | _ when is_letter l.ch ->
        let literal, l = read_identifier l in
        ({type'= Token.look_up_ident literal; literal}, l)
    | _ ->
        (newToken Token.ILLEGAL l.ch, read_char l)
  in
  token @@ eat_whitespace l
