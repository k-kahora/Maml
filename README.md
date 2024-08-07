### Writing an interpreter in ~~go~~ ocaml 
## TODO
- [x] pass read operands test case
- [x] pass string conversion test case
- [ ] Convert the interpreter errors(failwith) into results!
- [ ] figure out a better way to assert the byte value of each opcode, can lead to plenty of annoying, logic errors that are hard to catch
## Interesting
Noticed a case where if the last of the input is a {keyword|number} my loops do a index out of bounds error, however this was never recognized within the tests
## Features
- integers
- booleans
- strings
- arrays
- hashes
- prefix-, infix- and index operators
- conditionals
- global and local bindings
- first-class functions
- return statements
- closures
## Ultimate Goal
Provide some visualization of the *AST*, Tokenizer, Lexer, and *Evaluater* from the Interprter and *Virtual Machine*, (stack, registers) and *optimizer* from the compiler

## FIXME PRIORTY Sorted
- [ ] Evaluater is a mess of recursive functions need more modular formatting
- [ ] Need to add mli files for all ml files with proper name space exposure
- [ ] Reversing let statements but not block statements very weird
- [ ] Refractoring to results and options instead of failswith (asserts arent bad honostly for dev phase)
- [ ] Make every failswith return a Result instead of a runtime error return an error object using Result by Ocaml

## Future plans
* Use emojis
* Maybe change keywords to something obscene
* Add a unique feature that makes development hell
* Add less than or equal to and greater than respectively
* Make the langugage installable on Nix systems via the flake in your github
* Add the langugae to this project [monkeys in the wild](https://github.com/mrnugget/monkeylang/?tab=readme-ov-file#adding-a-new-implementation)
* Do a nix build of the language so that it can be installed with the flake 
* Monkey logo modification
* Macros sections
* CMD line interprter to run commands
* .ape file extensions
* custom lsp for this toy langugae
* instructions for running the language
# Priority
Command line runner to run Monkey code, .ape extensions, Very nice Readme, nixos compatable, implement hash tables, Chapter5, 

# Bugs
* Only one print statement triggers at a time in a file



# Copiler Must
* Work on the readme as you go
* Always have mli files
* Always have internal documentation
* Always have tail call optimization checks
* Also use functors and monads to dive deeper into the functional B.S.
