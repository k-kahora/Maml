(* Define a function to simulate evaluation of input *)
open Lex

let ( let* ) = Result.bind

let rec evaluate (l : lexer) : unit =
  let _ = Token.token_to_string_debug Token.EOF in
  let token, lex = Lex.next_token l in
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

let env = Object.Environment.new_environment ()

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
    let _error_print a = Code.CodeError.print_error a in
    Result.fold
      ~error:(fun a -> Code.CodeError.print_error a)
      ~ok:(fun a -> Object.Obj.item_to_string a |> print_endline)
      (operate_machine input) ;
    repl ()

and operate_machine input =
  let l = Lex.new' input in
  let p = Parsing.new_parser l in
  let program = Parsing.parse_program p in
  let* compiler = Compiler.compile program.statements Compiler.new_compiler in
  let machine = Vm.new_virtual_machine compiler in
  let* result = Vm.run machine in
  let stack_elem = result.last_item_poped in
  Ok stack_elem

let () = repl ()
