# Malcolm Kahora
## _Graffiti_

[![N|Solid](https://cldup.com/dTxpPi9lDf.thumb.png)](https://nodesource.com/products/nsolid)

[![Build Status](https://travis-ci.org/joemccann/dillinger.svg?branch=master)](https://travis-ci.org/joemccann/dillinger)

Graffiti is a interprted and compiled language written in Ocaml.

- Type graffiti code
- See the AST
- ✨Visualize walking the AST in real time
- Look at the byte code being produced!
## Features
- Primitives (integers, booleans, strings, arrays, hashes)
- Conditionals
- First class functons
- Closusers, (Inteprted only)

## In Progress

This is designed to be a learning language, so I want the compiler to be able to output a well structured Abstract Syntax Tree, Easy to read bytecode and a visual way to see the program walk the ast or bytecode.

> The cmdline tool will let you either compile 
> or interpret it, as well as output an AST or the ByteCode.
> The idea is to visualize the inher workings of a 
> Programming language

For many it is a mystery how programming languages work, the goal of this project was to demistify that for myself as well as share what I learned with other, additionaly I wanted to learn more about functional programming and ocaml seemed like a great choice.

## Tech

Dillinger uses a number of open source projects to work properly:

- [Ocaml] - An industrial-strength functional programming language with an emphasis on expressiveness and safety 
- [Alcotest] - Alcotest exposes a simple interface to perform unit tests
- [xxhash] - xxHash is an Extremely fast Hash algorithm, processing at RAM speed limits.


## Installation

*TODO* Add install instructions for Nix and then everyone else

```sh
cd dillinger
nix shell graffiti
graffiti --version
```

| Plugin | README |
| ------ | ------ |
| Dropbox | [plugins/dropbox/README.md][PlDb] |
| GitHub | [plugins/github/README.md][PlGh] |
| Google Drive | [plugins/googledrive/README.md][PlGd] |
| OneDrive | [plugins/onedrive/README.md][PlOd] |
| Medium | [plugins/medium/README.md][PlMe] |
| Google Analytics | [plugins/googleanalytics/README.md][PlGa] |

## Inspiration
- [token-ast-node]
- [d3-visualizer]
- [clean-formatter]
- [sql-query]
- [Gleam]

## Development

Open your terminal

### issues

- [ ] Evaluater is a mess of recursive functions need more modular formatting
- [ ] Need to add mli files for all ml files with proper name space exposure
- [ ] Reversing let statements but not block statements very weird
- [ ] Refractoring to results and options instead of failswith (asserts arent bad honostly for dev phase)
- [ ] Make every failswith return a Result instead of a runtime error return an error object using Result by Ocaml

#### future plans
- STEP 1: Finish the compiler and interpreter as well as a useful cmdline utility to use both (repl, ast outpus, bytecode output)
- STEP 2: Website with lang info as well as breif tutorial
- STEP 3: AST visualizer
- STEP 4: Bytecode visualizer


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
* Add a CI/CD script to auto run tests on push
* Like gleam create a tutorial to learn the language

### standards
#### Copiler Must Have

* Always have mli files
* Always have internal documentation
* Always have tail call optimization checks
* Always remove all warning silencers before release
* No exceptions only use Results and Result.bind



#### Building for source


## License

MIT

**Free Software, Yeah!**

[//]: # (These are reference links used in the body of this note and get stripped out when the markdown processor does its job. There is no need to format nicely because it shouldn't be seen. Thanks SO - http://stackoverflow.com/questions/4823468/store-comments-in-markdown-syntax)

   [dill]: <https://github.com/joemccann/dillinger>
   [git-repo-url]: <https://github.com/joemccann/dillinger.git>
   [john gruber]: <http://daringfireball.net>
   [df1]: <http://daringfireball.net/projects/markdown/>
   [markdown-it]: <https://github.com/markdown-it/markdown-it>
   [Ace Editor]: <http://ace.ajax.org>
   [node.js]: <http://nodejs.org>
   [Twitter Bootstrap]: <http://twitter.github.com/bootstrap/>
   [jQuery]: <http://jquery.com>
   [@tjholowaychuk]: <http://twitter.com/tjholowaychuk>
   [express]: <http://expressjs.com>
   [AngularJS]: <http://angularjs.org>
   [Ocaml]: <https://ocaml.org/>
   [Gleam]: <https://ocaml.org/>
   [Alcotest]: <https://github.com/mirage/alcotest>
   [xxhash]: <https://github.com/Cyan4973/xxHash>
   [token-ast-node]: <https://resources.jointjs.com/demos/rappid/apps/Ast/index.html>
   [d3-visualizer]: <https://observablehq.com/@aarebecca/ast-explorer>
   [clean-formatter]: <https://viswesh.github.io/astVisualizer/index.html>
   [sql-query]: <https://observablehq.com/@john-guerra/sql-query-visualizer>

   [PlDb]: <https://github.com/joemccann/dillinger/tree/master/plugins/dropbox/README.md>
   [PlGh]: <https://github.com/joemccann/dillinger/tree/master/plugins/github/README.md>
   [PlGd]: <https://github.com/joemccann/dillinger/tree/master/plugins/googledrive/README.md>
   [PlOd]: <https://github.com/joemccann/dillinger/tree/master/plugins/onedrive/README.md>
   [PlMe]: <https://github.com/joemccann/dillinger/tree/master/plugins/medium/README.md>
   [PlGa]: <https://github.com/RahulHP/dillinger/blob/master/plugins/googleanalytics/README.md>
