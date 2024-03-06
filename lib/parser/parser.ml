type parser = {l: Lexer.lexer; curToken: Token.token; peekToken: Token.token}

let next_token (p : parser) : parser =
  let peekToken, l = Lexer.next_token p.l in
  {curToken= peekToken; peekToken; l}

let new_parser (l : Lexer.lexer) : parser =
  let curToken, cur = Lexer.next_token l in
  let peekToken, l = Lexer.next_token cur in
  {l; curToken; peekToken}

let parse_program () = {Ast.statements= []}
