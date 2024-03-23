### Writing an interpreter in ~~go~~ ocaml 
## TODO
- [ ] # Refractor the main.ml to repl
- [ ] Make a trace ability for the parser so you can trace through an expression being parsed
- [ ] Clean up the token code seems pretty redeundent to me, (maybe I do not have to convert to a string)
## Interesting
Noticed a case where if the last of the input is a {keyword|number} my loops do a index out of bounds error, however this was never recognized within the tests

## 
