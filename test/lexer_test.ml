let test_token (l : Lexer.lexer)
    ((expectedType, expectedLiteral) : Token.token_name * string) : Lexer.lexer
    =
  let tok, new_l = Lexer.next_token l in
  Alcotest.check
    (Alcotest.pair Alcotest.string Alcotest.string)
    "Token type matches"
    (Token.token_to_string expectedType, expectedLiteral)
    (Token.token_to_string tok.type', tok.literal) ;
  new_l

let test_next_token () =
  let input =
    {|let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5
5 < 10 > 5

if (5 < 10) {
     return true;
} else {
     return false;
}
10 == 10;
10 != 9;
10 >= 9;
9 <= 10;
"foobar";
"foo bar";
[1, 2];
|}
  in
  let tests =
    [ (Token.LET, "let")
    ; (Token.IDENT, "five")
    ; (Token.ASSIGN, "=")
    ; (Token.INT, "5")
    ; (Token.SEMICOLON, ";")
    ; (Token.LET, "let")
    ; (Token.IDENT, "ten")
    ; (Token.ASSIGN, "=")
    ; (Token.INT, "10")
    ; (Token.SEMICOLON, ";")
    ; (Token.LET, "let")
    ; (Token.IDENT, "add")
    ; (Token.ASSIGN, "=")
    ; (Token.FUNCTION, "fn")
    ; (Token.LPAREN, "(")
    ; (Token.IDENT, "x")
    ; (Token.COMMA, ",")
    ; (Token.IDENT, "y")
    ; (Token.RPAREN, ")")
    ; (Token.LBRACE, "{")
    ; (Token.IDENT, "x")
    ; (Token.PLUS, "+")
    ; (Token.IDENT, "y")
    ; (Token.SEMICOLON, ";")
    ; (Token.RBRACE, "}")
    ; (Token.SEMICOLON, ";")
    ; (Token.LET, "let")
    ; (Token.IDENT, "result")
    ; (Token.ASSIGN, "=")
    ; (Token.IDENT, "add")
    ; (Token.LPAREN, "(")
    ; (Token.IDENT, "five")
    ; (Token.COMMA, ",")
    ; (Token.IDENT, "ten")
    ; (Token.RPAREN, ")")
    ; (Token.SEMICOLON, ";")
    ; (Token.BANG, "!")
    ; (Token.MINUS, "-")
    ; (Token.SLASH, "/")
    ; (Token.ASTERISK, "*")
    ; (Token.INT, "5")
    ; (Token.INT, "5")
    ; (Token.LT, "<")
    ; (Token.INT, "10")
    ; (Token.GT, ">")
    ; (Token.INT, "5")
    ; (Token.IF, "if")
    ; (Token.LPAREN, "(")
    ; (Token.INT, "5")
    ; (Token.LT, "<")
    ; (Token.INT, "10")
    ; (Token.RPAREN, ")")
    ; (Token.LBRACE, "{")
    ; (Token.RETURN, "return")
    ; (Token.TRUE, "true")
    ; (Token.SEMICOLON, ";")
    ; (Token.RBRACE, "}")
    ; (Token.ELSE, "else")
    ; (Token.LBRACE, "{")
    ; (Token.RETURN, "return")
    ; (Token.FALSE, "false")
    ; (Token.SEMICOLON, ";")
    ; (Token.RBRACE, "}")
    ; (Token.INT, "10")
    ; (Token.EQ, "==")
    ; (Token.INT, "10")
    ; (Token.SEMICOLON, ";")
    ; (Token.INT, "10")
    ; (Token.NOT_EQ, "!=")
    ; (Token.INT, "9")
    ; (Token.SEMICOLON, ";")
    ; (Token.INT, "10")
    ; (Token.GTEQ, ">=")
    ; (Token.INT, "9")
    ; (Token.SEMICOLON, ";")
    ; (Token.INT, "9")
    ; (Token.LTEQ, "<=")
    ; (Token.INT, "10")
    ; (Token.SEMICOLON, ";")
    ; (Token.STRING, "foobar")
    ; (Token.SEMICOLON, ";")
    ; (Token.STRING, "foo bar")
    ; (Token.SEMICOLON, ";")
    ; (Token.LBRACKET, "[")
    ; (Token.INT, "1")
    ; (Token.COMMA, ",")
    ; (Token.INT, "2")
    ; (Token.RBRACKET, "]")
    ; (Token.SEMICOLON, ";")
    ; (Token.EOF, "\000") ]
  in
  let l = Lexer.new' input in
  ignore (List.fold_left test_token l tests)

let () =
  Alcotest.run "Token tests"
    [ ( "Next Token"
      , [Alcotest.test_case "Test Next token" `Quick test_next_token] ) ]
