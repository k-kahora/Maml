(* Define a function to simulate evaluation of input *)
open Lexer

let rec evaluate (l : lexer) : unit =
  let _ = Token.token_to_string_debug Token.EOF in
  let token, lex = Lexer.next_token l in
  match token.type' with
  | Token.EOF ->
      print_endline "miss"
  | _ ->
      print_endline
        ( "{Token:"
        ^ Token.token_to_string_debug token.type'
        ^ ", Literal:" ^ token.literal ^ "}" ) ;
      evaluate lex

(* Define the REPL function *)

let env = Environment.new_environment ()

let rec repl () =
  print_string "==> " ;
  (* Print prompt *)
  flush stdout ;
  (* Ensure the prompt is displayed *)
  let input = read_line () in
  (* Read a line of input from the user *)
  if input = "exit" then
    (* Check if the input is a command to exit *)
    print_endline "Goodbye!"
  else
    (* FIXME NOTE This code failwis unless an int is passed *)
    let l = Lexer.new' input in
    let p = Parser.new_parser l in
    let program = Parser.parse_program p in
    (* print_endline (Ast.program_str program) ; *)
    let evaluated = Evaluater.eval env program in
    print_endline @@ Object.item_to_string evaluated ;
    repl ()

let () = repl ()

