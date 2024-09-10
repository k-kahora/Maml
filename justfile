set positional-arguments

@graffiti *args='':
  dune exec -- Graffiti "$@"
@eval *args='':
  dune exec -- Graffiti --repl --eval="$@"

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