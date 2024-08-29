set positional-arguments

@graffiti *args='':
  dune exec -- Graffiti "$@"

repl:
	dune exec Interpreter
build:
	dune build
utop:
	utop -init ./bin/inti_script.ml
test:
	dune runtest 