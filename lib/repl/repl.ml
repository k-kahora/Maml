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

let constants = Compiler.empty_constants ()

let globals = Vm.empty_globals ()

let symbol_table_with_built_ins () =
  let _, symbol_table =
    List.fold_left
      (fun (idx, symbol_table) (name, _fn) ->
        let _, new_table = Symbol_table.define_builtin idx name symbol_table in
        (idx + 1, new_table) )
      (0, Symbol_table.new_symbol_table ())
      Object.Builtin.builtins
  in
  symbol_table

let symbol_table = symbol_table_with_built_ins ()

let rec repl run_or_comp output_mode prompt state =
  print_string (prompt ^ " ") ;
  (*print_string :>*)
  (* Print prompt *)
  flush stdout ;
  (* Ensure the prompt is displayed *)
  let input = read_line () in
  (* Read a line of input from the user *)
  let result =
    match run_or_comp with
    | `Compiled ->
        operate_machine_compile state input
    | `Interpret ->
        operate_machine_interpret state input
  in
  let _ =
    match output_mode with
    | `Default ->
        ()
    | `Ast ->
        ()
    | `Byte ->
        ()
    | `Both ->
        ()
  in
  (* FIXME NOTE This code failwis unless an int is passed *)
  Result.fold
    ~error:(fun err ->
      Code.CodeError.print_error err ;
      repl run_or_comp output_mode prompt state )
    ~ok:(fun (item, state) ->
      Object.Obj.item_to_string item |> print_endline ;
      repl run_or_comp output_mode prompt state )
    result

and[@ocaml.warning "-27-26"] operate_machine_compile state input =
  let symbol_table, constants, globals = state in
  let l = Lex.new' input in
  let p = Parsing.new_parser l in
  let program = Parsing.parse_program p in
  let fresh_compiler = Compiler.new_with_state symbol_table constants in
  let* compiler = Compiler.compile program.statements fresh_compiler in
  let machine = Vm.new_with_global_store compiler globals in
  let* vm = Vm.run machine in
  let stack_elem = Vm.last_item_popped vm in
  let state = (compiler.symbol_table, compiler.constants, vm.globals) in
  Ok (stack_elem, state)

and[@ocaml.warning "-27-26"] operate_machine_interpret state input =
  let l = Lex.new' input in
  let p = Parsing.new_parser l in
  let program = Parsing.parse_program p in
  (* print_endline (Ast.program_str program) ; *)
  let evaluated = Evaluater.eval env program in
  Ok (evaluated, state)

let boot_into_repl ?(run_or_comp = `Compiled) ?(output_mode = `Default)
    ?(prompt = "==>") () =
  repl run_or_comp output_mode prompt (symbol_table, constants, globals)
