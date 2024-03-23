type parser =
  { l: Lexer.lexer
  ; curToken: Token.token
  ; peekToken: Token.token
  ; prefixParseFns: (Token.token_name, prefixParseFn) Utils.Token_AssocList.t
  ; infinxParseFns: (Token.token_name, infixParseFn) Utils.Token_AssocList.t
  ; errors: string list }

and prefixParseFn = parser -> Ast.expression * parser

and infixParseFn = parser -> Ast.expression -> Ast.expression * parser

type precedence =
  | LOWEST
  | EQUALS (* == *)
  | LESSGREATER (* < or > *)
  | SUM (* + *)
  | PRODUCT (* * *)
  | PREFIX (* -X or !X *)
  | CALL (* func(X) *)

let precedences =
  Utils.Token_AssocList.(
    empty |> add Token.EQ EQUALS |> add Token.NOT_EQ EQUALS
    |> add Token.LT LESSGREATER |> add Token.GT LESSGREATER
    |> add Token.PLUS SUM |> add Token.MINUS SUM |> add Token.SLASH PRODUCT
    |> add Token.ASTERISK PRODUCT )

let precedence_level (p : precedence) =
  match p with
  | LOWEST ->
      0
  | EQUALS ->
      1
  | LESSGREATER ->
      2
  | SUM ->
      3
  | PRODUCT ->
      4
  | PREFIX ->
      5
  | CALL ->
      6

let cur_precedence (p : parser) : precedence =
  match Utils.Token_AssocList.find p.curToken.type' precedences with
  | Some pr ->
      pr
  | None ->
      LOWEST

let peek_precedence (p : parser) : precedence =
  match Utils.Token_AssocList.find p.peekToken.type' precedences with
  | Some pr ->
      pr
  | None ->
      LOWEST

let register_infix (p : parser) ~(t : Token.token_name) ~(fn : infixParseFn) :
    parser =
  {p with infinxParseFns= Utils.Token_AssocList.add t fn p.infinxParseFns}

let register_prefix (p : parser) ~(t : Token.token_name) ~(fn : prefixParseFn) :
    parser =
  {p with prefixParseFns= Utils.Token_AssocList.add t fn p.prefixParseFns}

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

(* TODO trace through 1 + 2 * 3 to understand operator precedenc *)
let parse_expression (precedence' : precedence) (p : parser) :
    Ast.expression * parser =
  let prefix =
    match Utils.Token_AssocList.find p.curToken.type' p.prefixParseFns with
    | Some pre ->
        pre
    | None ->
        failwith
          ( "No prefix parse function registed for "
          ^ Token.token_to_string_debug p.curToken.type'
          ^ " operator" )
  in
  let left_exp, p = prefix p in
  (* Loop while the peekToken is not a semicolon and the precedence is less than the peek precedenc *)
  let rec loop left_e p =
    match p.curToken.type' with
    | Token.SEMICOLON ->
        (left_e, p)
    | _
      when precedence_level precedence' < precedence_level @@ peek_precedence p
      -> (
      match Utils.Token_AssocList.find p.peekToken.type' p.infinxParseFns with
      | Some infix ->
          (* NOTE this is where the magic happens *)
          (* for 1 + 2 + 3 this returns [Ast.InfixExpression (1 + 2)] for exp*)
          (* This is where less precedenc expressions get sucked into the left arm *)
          let exp, p = infix (next_token p) left_e in
          loop exp p
      | None ->
          (left_e, p) )
    | _ ->
        (* This is triggered for right binding No expression will be put into a left arm if this match statement is trigged *)
        (left_e, p)
  in
  loop left_exp p

let parse_integer_literal (p : parser) : Ast.expression * parser =
  ( Ast.IntegerLiteral
      {token= p.curToken; value= int_of_string p.curToken.literal}
  , p )

(* let lit = int_of_string p.curToken.literal in *)

let parse_prefix_expression (p : parser) : Ast.expression * parser =
  let expression, new_p = next_token p |> parse_expression PREFIX in
  ( Ast.PrefixExpression
      {token= p.curToken; operator= p.curToken.literal; right= expression}
  , new_p )

let parse_identifier (p : parser) : Ast.expression * parser =
  (Ast.Identifier {token= p.curToken; value= p.curToken.literal}, p)

let parse_infix_expression (p : parser) (left_exp : Ast.expression) :
    Ast.expression * parser =
  let right_exp, new_p = next_token p |> parse_expression (cur_precedence p) in
  ( Ast.InfixExpression
      { left= left_exp
      ; token= p.curToken
      ; operator= p.curToken.literal
      ; right= right_exp }
  , new_p )

let new_parser (l : Lexer.lexer) : parser =
  let curToken, cur = Lexer.next_token l in
  let peekToken, l = Lexer.next_token cur in
  { l
  ; curToken
  ; peekToken
  ; errors= []
  ; prefixParseFns= Utils.Token_AssocList.empty
  ; infinxParseFns= Utils.Token_AssocList.empty }
  |> register_prefix ~t:Token.IDENT ~fn:parse_identifier
  |> register_prefix ~t:Token.INT ~fn:parse_integer_literal
  |> register_prefix ~t:Token.BANG ~fn:parse_prefix_expression
  |> register_prefix ~t:Token.MINUS ~fn:parse_prefix_expression
  |> register_infix ~t:Token.PLUS ~fn:parse_infix_expression
  |> register_infix ~t:Token.MINUS ~fn:parse_infix_expression
  |> register_infix ~t:Token.SLASH ~fn:parse_infix_expression
  |> register_infix ~t:Token.ASTERISK ~fn:parse_infix_expression
  |> register_infix ~t:Token.EQ ~fn:parse_infix_expression
  |> register_infix ~t:Token.NOT_EQ ~fn:parse_infix_expression
  |> register_infix ~t:Token.LT ~fn:parse_infix_expression
  |> register_infix ~t:Token.GT ~fn:parse_infix_expression

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

(* See if a parsing function is associated with the token and call that function *)

let parse_expression_statement p =
  let expr, p = parse_expression LOWEST p in
  let stmt =
    let tok = {Token.type'= Token.ILLEGAL; literal= "null"} in
    Ast.Expressionstatement {token= tok; expression= (*NOTE ==> *) expr}
  in
  let p = if peek_token_is p Token.SEMICOLON then next_token p else p in
  (* Skip semeicolons*)
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
      Some (parse_expression_statement p)

let parse_program (p : parser) : Ast.program =
  (* Parse each token until there is a EOF token *)
  let rec looper acc p =
    match p.curToken.type' with
    | Token.EOF ->
        acc
    | _ -> (
      match parse_statement p with
      | None ->
          failwith "Unreachable error"
      | Some (stmt, p) ->
          looper (stmt :: acc) (next_token p) )
  in
  let d_stms = looper [] p in
  {Ast.statements= (* List.rev *) d_stms}
(* FIXME reversing affects runtime  *)
(* List must be reversed due to the way is is handled by appending to the front *)
