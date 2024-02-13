type test_variant = {expectedType: Token.token_name; expectedLiteral: string}

let test_token (l : Lexer.lexer) {expectedType; expectedLiteral} : Lexer.lexer =
  let new_l, tok = Lexer.next_token l in
  Alcotest.check
    (Alcotest.pair Alcotest.string Alcotest.string)
    "Token type matches"
    (Token.token_to_string expectedType, tok.literal)
    (Token.token_to_string tok.type', expectedLiteral) ;
  new_l

let test_next_token () =
  let input = "=+(){},;+" in
  let tests =
    [ {expectedType= Token.ASSIGN; expectedLiteral= "="}
    ; {expectedType= Token.PLUS; expectedLiteral= "+"}
    ; {expectedType= Token.LPAREN; expectedLiteral= "("}
    ; {expectedType= Token.RPAREN; expectedLiteral= ")"}
    ; {expectedType= Token.LBRACE; expectedLiteral= "{"}
    ; {expectedType= Token.RBRACE; expectedLiteral= "}"}
    ; {expectedType= Token.COMMA; expectedLiteral= ","}
    ; {expectedType= Token.SEMICOLON; expectedLiteral= ";"}
    ; {expectedType= Token.PLUS; expectedLiteral= "+"}
    ; {expectedType= Token.EOF; expectedLiteral= "\x00"} ]
  in
  let l = Lexer.new' input in
  ignore (List.fold_left test_token l tests)

let () =
  Alcotest.run "My tests"
    [ ( "Next Token"
      , [Alcotest.test_case "Test Next token" `Quick test_next_token] ) ]
