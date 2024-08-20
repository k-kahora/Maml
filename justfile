set positional-arguments

@freak *args='':
  dune exec -- Freakyscript "$@"

repl:
	dune exec Interpreter
build:
	dune build
utop:
	utop -init ./bin/inti_script.ml
test:
	dune runtest 