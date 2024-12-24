open Cmdliner
open Lwt

(* open Cohttp *)
open Cohttp_lwt_unix

(** [maml] features of graffiti will be as follows
    
--ast will always print out the ast in the repl
--bytecode will always print out the byte code in the repl
--bytecode & --ast will always print out the byte code and the ast in the repl
    
:> graffiti --ast --bytecode
:> graffiti --bytecode
:> graffiti --ast


*)

let maml repl interpret ast bytecode prompt literal =
  let run_or_comp = if interpret then `Interpret else `Compiled in
  if literal <> "" then Repl.execute_string literal |> print_endline
  else
    let output_mode =
      match (ast, bytecode) with
      | true, true ->
          `Both
      | false, true ->
          `Byte
      | true, false ->
          `Ast
      | false, false ->
          `Default
    in
    match repl with
    | true ->
        Repl.boot_into_repl ~prompt ~run_or_comp ~output_mode ()
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

let literal =
  let doc = "Give some maml code literal to be run" in
  Arg.(value & opt string "" & info ["e"; "eval"] ~doc)

(* let msg = *)
(*   let env = *)
(*     let doc = "Overrides the default message to print." in *)
(*     Cmd.Env.info "CHORUS_MSG" ~doc *)
(*   in *)
(*   let doc = "The message to print." in *)
(*   Arg.(value & pos 0 string "Revolt!" & info [] ~env ~docv:"MSG" ~doc) *)

let graffiti_t =
  Term.(const maml $ repl $ interpret $ ast $ bytecode $ prompt $ literal)

let cmd =
  let doc = "A toy language for visualizing the AST, and the bytecode" in
  let man =
    [`S Manpage.s_bugs; `P "Email bug reports to <bugs@example.org>."]
  in
  let info = Cmd.info "maml" ~version:"%‌%VERSION%%" ~doc ~man in
  Cmd.v info graffiti_t

(* let main () = exit (Cmd.eval cmd) *)
let process_input input =
  Printf.sprintf "Output: %s" @@ Repl.execute_string input

let callback _conn _req body =
  body |> Cohttp_lwt.Body.to_string >|= process_input
  >>= fun response -> Server.respond_string ~status:`OK ~body:response ()

let server = Server.create ~mode:(`TCP (`Port 8080)) (Server.make ~callback ())

(* let () = main () *)
let () = Lwt_main.run server

(* You can run the compiler with *)
(* curl -X POST http://localhost:8080/ -d "let str = \"monkey man\"; puts(str)" *)
