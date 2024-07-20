open Core

let env = Object.Environment.new_environment ()

let checksum_from_string buf =
  Md5.digest_string buf |> Md5.to_hex |> print_endline

let evaluate_program input =
  let l = Lex.new' input in
  let p = Parsing.new_parser l in
  let program = Parsing.parse_program p in
  (* print_endline (Ast.program_str program) ; *)
  Evaluater.eval env program

let read_string str =
  let evaluated = evaluate_program str in
  print_endline @@ Object.Obj.item_to_string evaluated

let repl () =
  let rec repl' () =
    print_string "==> " ;
    (* Print prompt *)
    Out_channel.(flush stdout) ;
    (* Ensure the prompt is displayed *)
    let input = In_channel.(input_line_exn stdin) in
    (* Read a line of input from the user *)
    (* if input = "exit" then *)
    (* Check if the input is a command to exit *)
    (* print_endline "Goodbye!" *)
    (* else *)
    (* FIXME NOTE This code failwis unless an int is passed *)
    let evaluated = evaluate_program input in
    print_endline @@ Object.Obj.item_to_string evaluated ;
    repl' ()
  in
  print_string
    {|
     Welcome to Freakyscript 😤😡😠🤬😈👿💀☠️
                                                
                                                
                ▒▒▒▒▒▒▒▒    ▒▒▒▒▒▒▒▒            
              ▒▒▒▒      ▒▒▒▒▒▒      ▒▒          
            ▒▒▒▒  ▒▒▒▒▒▒▒▒▒▒  ▒▒▒▒▒▒▒▒          
          ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒        
          ▒▒▒▒      ▒▒▒▒▒▒      ▒▒▒▒▒▒▒▒        
      ▒▒▒▒▒▒                      ▒▒▒▒▒▒▒▒      
    ▒▒▒▒██████        ████        ██▒▒▒▒▒▒▒▒    
  ▒▒▒▒▒▒██████▒▒▒▒▒▒████████▒▒▒▒▒▒▒▒████▒▒▒▒▒▒  
    ▒▒▒▒██▒▒▒▒▒▒▒▒████▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██▒▒▒▒    
      ▒▒▒▒▒▒  ▒▒████▒▒▒▒▒▒▒▒    ▒▒▒▒██▒▒▒▒      
      ▒▒▒▒    ▒▒██▒▒▒▒▒▒▒▒    ▒▒▒▒██▒▒▒▒        
    ▒▒▒▒    ▒▒██▒▒▒▒▒▒▒▒    ▒▒▒▒██▒▒▒▒          
  ▒▒▒▒▒▒  ▒▒██▒▒▒▒▒▒▒▒▒▒  ▒▒▒▒██▒▒▒▒▒▒          
  ▒▒▒▒    ▒▒██▒▒▒▒▒▒▒▒    ▒▒▒▒██▒▒▒▒            
▒▒▒▒▒▒    ████▒▒▒▒▒▒▒▒    ▒▒██▒▒▒▒              
▒▒▒▒▒▒    ▒▒▒▒▒▒▒▒▒▒    ▒▒▒▒██▒▒▒▒              
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒    ▒▒██▒▒▒▒                
  ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒    ▒▒▒▒██▒▒                  
  ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██                      
    ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██                        
      ▒▒▒▒▒▒▒▒▒▒▒▒▒▒██                          
          ▒▒▒▒▒▒▒▒                              
                                                
                                                


|} ;
  repl' ()

let read_file filename =
  let evaluate contents =
    let evaluated = evaluate_program contents in
    print_endline @@ Object.Obj.item_to_string evaluated
  in
  match filename with
  | "@" ->
      ()
  | "-" ->
      evaluate @@ In_channel.input_all In_channel.stdin
  | filename ->
      evaluate @@ In_channel.read_all filename

let command =
  Command.basic ~summary:"Run the freakyscript interpreter"
    (let%map_open.Command use_string =
       flag "-s" (optional string) ~doc:"string Provide freakyscript code"
     and _trial = flag "-t" no_arg ~doc:"run a built-in time trial"
     and repl_flag = flag "-r" no_arg ~doc:"run the monkey repl"
     and filename =
       anon (maybe_with_default "@" ("filename" %: Filename_unix.arg_type))
     in
     fun () ->
       (* if trial then printf "Running time trial\n" else *)
       if repl_flag then repl () else read_file filename ;
       match use_string with Some buf -> read_string buf | None -> () )

let () = Command_unix.run command
