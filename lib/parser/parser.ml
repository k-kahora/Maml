module type AssocList = sig
  type ('a, 'b) t

  val empty : ('a, 'b) t

  val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
end

type prefixParseFn = unit -> Ast.expression

type infixParseFn = Ast.expression -> Ast.expression

module Token_AssocList : AssocList = struct
  type ('a, 'b) t = ('a * 'b) list

  let empty = []

  let add key value list = (key, value) :: list

  (* let mem key list = List.exists (fun (a, _) -> a = key) list *)
end

type parser =
  { l: Lexer.lexer
  ; curToken: Token.token
  ; peekToken: Token.token
  ; prefixParseFns: (Token.token_name, prefixParseFn) Token_AssocList.t
  ; infinxParseFns: (Token.token_name, infixParseFn) Token_AssocList.t
  ; errors: string list }

let register_infix (p : parser) (t : Token.token_name) (fn : prefixParseFn) :
    parser =
  {p with infixParseFns= Token_AssocList.add t fn p.infinxParseFns}

let register_prefix (p : parser) (t : Token.token_name) (fn : prefixParseFn) :
    parser =
  {p with prefixParseFns= Token_AssocList.add t fn p.prefixParseFns}

let errors (p : parser) : string list = p.errors

let check_error a = match a with Ok _ -> a | Error e -> failwith e

let peek_error (p : parser) (t : Token.token_name) : string =
  let msg =
    Format.sprintf "expected next token to be %s got %s instead"
      (Token.token_to_string_debug t)
      (Token.token_to_string_debug p.peekToken.type')
  in
  msg
(* HACK Anti pattern copies the whole list each time*)

let print_parser (p : parser) : unit =
  Format.printf "Current Token: %s \n"
    (Token.token_to_string_debug p.curToken.type') ;
  Format.printf "Peek token: %s \n"
    (Token.token_to_string_debug p.peekToken.type')

let next_token (p : parser) : parser =
  let nextToken, l = Lexer.next_token p.l in
  (* Format.printf "Next Token: %s \n" *)
  (* Token.token_to_string_debug nextToken.type' ; *)
  {p with curToken= p.peekToken; peekToken= nextToken; l}

let new_parser (l : Lexer.lexer) : parser =
  let curToken, cur = Lexer.next_token l in
  let peekToken, l = Lexer.next_token cur in
  { l
  ; curToken
  ; peekToken
  ; errors= []
  ; prefixParseFns= Token_AssocList.empty
  ; infinxParseFns= Token_AssocList.empty }

module type Monad = sig
  type 'a t

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Maybe : Monad = struct
  type 'a t = 'a option

  let return x = Some x

  let ( >>= ) m f = match m with None -> None | Some x -> f x
end

let cur_token_is (p : parser) (t : Token.token_name) : bool =
  p.curToken.type' = t

let peek_token_is (p : parser) (t : Token.token_name) : bool =
  p.peekToken.type' = t

let expect_peek (p : parser) (t : Token.token_name) : (parser, string) result =
  let b = peek_token_is p t in
  if b then Ok (next_token p) else Error (peek_error p t)

let rec skip_expression nxt =
  match nxt with
  | pst when cur_token_is pst Token.SEMICOLON ->
      nxt
  | _ ->
      skip_expression (next_token nxt)

(* all the bindings will fail if the incorrect token is found *)
let parse_return_statement (p : parser) : Ast.statement * parser =
  let stmt =
    Ast.Returnstatement
      { token= p.curToken
      ; return_value= Identifier {token= p.curToken; value= "null"} }
  in
  let p = skip_expression p in
  (stmt, p)

(* all the bindings will fail if the incorrect token is found *)
let parse_let_statement (p : parser) : Ast.statement * parser =
  let open Result in
  let ( >>= ) = bind in
  let stmt =
    Ast.Letstatement
      { token= p.curToken
      ; name= {token= p.curToken; value= "null"}
      ; value= Identifier {token= p.curToken; value= "null"} }
  in
  (* First check for the ident token *)
  let last_token =
    Ok p >>= fun ft -> expect_peek ft Token.IDENT |> check_error
  in
  (* Set statement name = to the current token *)
  let stmt =
    match stmt with
    | Ast.Letstatement st ->
        Ast.Letstatement
          { st with
            name=
              { token= (get_ok last_token).curToken
              ; value= (get_ok last_token).curToken.literal } }
    | _ ->
        failwith "FIXME This setup is super janky"
  in
  (* check for the ASSIGN token *)
  let last_token =
    last_token >>= fun nt -> expect_peek nt Token.ASSIGN |> check_error
  in
  (stmt, skip_expression @@ get_ok last_token)
(* Ast.new_let_satement () *)

let parse_statement (p : parser) : (Ast.statement * parser) option =
  match p.curToken.type' with
  | Token.LET ->
      Some (parse_let_statement p)
  | Token.RETURN ->
      Some (parse_return_statement p)
  | _ ->
      None

let parse_program (p : parser) : Ast.program =
  (* Parse each token until there is a EOF token *)
  let rec looper acc p =
    match p.curToken.type' with
    | Token.EOF ->
        acc
    | _ -> (
      match parse_statement p with
      | None ->
          looper acc (next_token p)
      | Some (stmt, p) ->
          looper (stmt :: acc) (next_token p) )
  in
  let d_stms = looper [] p in
  {Ast.statements= List.rev d_stms}
(* List must be reversed due to the way is is handled by appending to the front *)
