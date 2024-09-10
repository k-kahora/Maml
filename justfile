set positional-arguments

@maml *args='':
  dune exec -- Maml "$@"
@eval *args='':
  dune exec -- Maml --repl --eval="$@"

repl:
	dune exec Interpreter
build:
	dune build
utop:
	utop -init ./bin/inti_script.ml
test:
	dune runtest 
	
docker_run:
	docker run -it graffiti-image:latest