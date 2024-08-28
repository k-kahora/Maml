open Cmdliner

(** [graffiti] features of graffiti will be as follows
    
--ast will always print out the ast in the repl
--bytecode will always print out the byte code in the repl
--bytecode & --ast will always print out the byte code and the ast in the repl
    
:> graffiti --ast --bytecode
:> graffiti --bytecode
:> graffiti --ast


*)

let graffiti repl interpret ast bytecode prompt =
  match repl with
  | true -> (
    match interpret with
    | true ->
        Repl.boot_into_repl ~prompt ~run_or_comp:`Interpret ()
    | false ->
        Repl.boot_into_repl ~prompt () )
  | false ->
      Format.printf "ast: %B, bytecode: %b" ast bytecode

let repl =
  let doc = "run the repl" in
  Arg.(value & flag & info ["r"; "repl"] ~doc)

let interpret =
  let doc = "compile output in the repl or intrepret it" in
  Arg.(value & flag & info ["i"; "interpret"] ~doc)

let ast =
  let doc = "Output the ast of each command in the repl" in
  Arg.(value & flag & info ["a"; "ast"] ~doc)

let prompt =
  let doc = "Custom string for the prompt defalt is ==>" in
  Arg.(value & opt string "==>" & info ["p"; "prompt"] ~doc)

let bytecode =
  let doc = "Output the bytecode of each command in the repl" in
  Arg.(value & flag & info ["b"; "bytecode"] ~doc)

(* let msg = *)
(*   let env = *)
(*     let doc = "Overrides the default message to print." in *)
(*     Cmd.Env.info "CHORUS_MSG" ~doc *)
(*   in *)
(*   let doc = "The message to print." in *)
(*   Arg.(value & pos 0 string "Revolt!" & info [] ~env ~docv:"MSG" ~doc) *)

let graffiti_t =
  Term.(const graffiti $ repl $ interpret $ ast $ bytecode $ prompt)

let cmd =
  let doc = "A toy language for visualizing the AST, and the bytecode" in
  let man =
    [`S Manpage.s_bugs; `P "Email bug reports to <bugs@example.org>."]
  in
  let info = Cmd.info "graffiti" ~version:"%‌%VERSION%%" ~doc ~man in
  Cmd.v info graffiti_t

let main () = exit (Cmd.eval cmd)

let () = main ()
