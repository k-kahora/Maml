### Writing an interpreter in ~~go~~ ocaml 
## TODO
- [ ] # Refractor the main.ml to repl
- [ ] Make a trace ability for the parser so you can trace through an expression being parsed
- [ ] Clean up the token code seems pretty redeundent to me, (maybe I do not have to convert to a string)
## Interesting
Noticed a case where if the last of the input is a {keyword|number} my loops do a index out of bounds error, however this was never recognized within the tests

## FIXME PRIORTY Sorted
- [ ] Evaluater is a mess of recursive functions need more modular formatting
- [ ] Need to add mli files for all ml files with proper name space exposure
- [ ] Reversing let statements but not block statements very weird
- [ ] Refractoring to results and options instead of failswith (asserts arent bad honostly for dev phase)

## Future plans
- [ ] Make a freaky branch (current ideas)
* Use emojis
* Maybe change keywords to something obscene
* Add a unique feature that makes development hell
