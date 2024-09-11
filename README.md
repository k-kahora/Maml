<!-- Improved compatibility of back to top link: See: https://github.com/othneildrew/Best-README-Template/pull/73 -->
<a id="readme-top"></a>
<!--
*** Thanks for checking out the Best-README-Template. If you have a suggestion
*** that would make this better, please fork the repo and create a pull request
*** or simply open an issue with the tag "enhancement".
*** Don't forget to give the project a star!
*** Thanks again! Now go create something AMAZING! :D
-->



<!-- PROJECT SHIELDS -->
<!--
*** I'm using markdown "reference style" links for readability.
*** Reference links are enclosed in brackets [ ] instead of parentheses ( ).
*** See the bottom of this document for the declaration of the reference variables
*** for contributors-url, forks-url, etc. This is an optional, concise syntax you may use.
*** https://www.markdownguide.org/basic-syntax/#reference-style-links
-->
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]



<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/othneildrew/Best-README-Template">
    <img src="images/maml.png" alt="Logo" width="1007" height="300">
  </a>

  <h3 align="center">Maml Programming Language</h3>

  <p align="center">
    An Awesome scripting language implemented in Ocaml 
    <br />
    <a href="https://github.com/k-kahora/Maml/issues/new?labels=bug&template=bug-report---.md">Report Bug</a>
    ·
    <a href="https://github.com/k-kahora/Maml/issues/new?labels=enhancement&template=feature-request---.md">Request Feature</a>
  </p>
</div>



<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
        <li><a href="#built-on">Built on</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project

This project is an interpreter and a compiler for the Monkey Programming Language. The interpreter features its own Lexer, Parser, and Evaluator. The compiler features its own 32-bit opcodes, a bytecode compiler to convert the AST into bytecode, and a virtual machine to execute the bytecode.

The books I followed, "Writing an Interpreter in Go" and "Writing a Compiler in Go," were very enjoyable and easy to follow due to the simple patterns the author created as well as the simplicity of Golang.

The goal of the projects was sevenfold and covered a lot of ground for things I have been wanting to learn. First and foremost, writing a compiled and interpreted language from the ground up. In addition, this is my first time writing OCaml, as well as using a functional language, so it was incredibly helpful in getting me really comfortable with the language. Additionally, unit testing was something I tended to avoid; however, having to convert all the Golang tests to OCaml tests was a whole project in and of itself. I do not regret implementing the tests as they give additional confidence in the robustness of the language.

Translating the Go code into OCaml code was never straightforward. Things such as loops, mutability, arrays, and early returns are not the best way to go about things in OCaml and are avoided. This involed using recursion, lists, and result monads to achieve the above.

Areas I have missed or lacked: I did not use the Jane Street Core library as I wanted to first learn the standard OCaml library. Also, print debugging was a nightmare in OCaml, having to write too many custom string functions for OCaml objects. In the future, I plan to look into PPX as I believe it is a solid solution to my printing nightmares.

As this is my first time writing OCaml code and a compiler/interpreter, please let me know of areas I may have missed the ball on or areas [I could have done better on](https://github.com/k-kahora/Maml/issues/new?labels=bug&template=bug-report---.md).

<p align="right">(<a href="#readme-top">back to top</a>)</p>

### Built With

* [![Ocaml][Ocaml.com]][Ocaml-url]

### Built On

* [![Nixos][Nixos-url]][Nixos.com]

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- GETTING STARTED -->
## Getting Started

Right now the easiest way to use this language is to use the nix package manager

### Prerequisites

To test drive Maml and jump straight into the repl
* Nix
  ```sh
  nix run github:k-kahora/maml/#maml -- -r
  ```
  
  
* To see what Maml can do
  ```sh
  nix run github:k-kahora/maml/#maml -- --help
  ```
  

### Installation from source

1. Clone the repo
   ```sh
   git clone https://github.com/k-kahora/writing-an-interpreter-in-ocaml
   ```
3. Install opam packages 
   ```sh
   # Make sure opam is installed before running these commands
   opam install xxhash
   opam install cmdliner
   opam install alcotest
   opam install dune
   ```
5. Run tests 

	``` sh
	dune runtest
	```

4. build the language
   ```sh
   dune build
   ```
5. Run the language
   ```sh
   dune exec -- Maml --help
   ```

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- USAGE EXAMPLES -->
## Usage
### feautures

| Feature                | Interpreter | Compiler |
|------------------------|-------------|----------|
| Bindings               | ✅          | ✅       |
| Conditionals           | ✅          | ✅       |
| Strings                | ✅          | ✅       |
| Integers               | ✅          | ✅       |
| arithmetic +-/*        | ✅          | ✅       |
| Arrays                 | ✅          | ✅       |
| Indexing               | ✅          | ✅       |
| Dictionarys            | ✅          | ✅       |
| Functions              | ✅          | ✅       |
| First class functions  | ✅          | ✅       |
| Higher order functions | ✅          | ✅       |
| Closures               | ✅          | ✅       |
| Recursion              | ✅          | ✅       |
| BuiltInFunctions       | ✅          | ✅       |
| Loops                  | ❌          | ❌       |
| Floats                 | ❌          | ❌       |
| Macros                 | ❌          | ❌       |
### Builtin functions

| Name         | Description                                               |
|--------------|-----------------------------------------------------------|
| len(x)       | Returns length of string,array,dictionary                 |
| first(x)     | Returns the first item of the array                       |
| last(x)      | Returns the last item of the array                        |
| rest(x)      | Returns every item but the first in an array              |
| push(x,item) | Appends item to the end of an array returning a new array |
| puts(x)      | Print x                                                   |

### small examples

These are small impractical examples curated to showcase the syntax of the language.

Bindings
```js
// You can use = or <- to bind
let x <- 30
let x = 30
```


Arithmatic
```js
let x <- 30
let y <- 20
x + y
// 50
```


Conditionals
```js
let value <- 50
let result <- if (value < 60) {value / 5} else {value / 2} 
// 10

```

Arrays
```js
let value <- [1,"hello",3,"world",5,[2,4,6],7,5,9]
let result_one <- value[1]
let result_two <- value[1 + 4][0]
// "hello"
// 2
```

Dictionarys
```js
let value <- {"monkey":{"see":{"monkey":{"do":"!"}}}}
let result = value["monkey"]["see"]["monkey"]["do"]
puts("monkey" + result)
// "monkey!"
```



Functions
```js
let sum <- fn(x,y) {x + y}
let result = sum(10,50)
let subtract_thirty = fn(item) {let thirty = 30; return item - 30}
let nested_square = fn() { fn(y) {  y * y } }
puts(nested_square()(subtract_thirty(result)))
// 900
```

Fibonacci sequence (Closures, and recursion)

```js
let fibonacci = fn(x) {
	if (x == 0) {
		return 0;
	} else {
		if (x == 1) {
			return 1;
		} else {
			fibonacci(x - 1) + fibonacci(x - 2);
		}
	}
};
puts(fibonacci(15));
// 610

```

Map

```js
let map = fn(list,f) {
  let iter = fn(list, acc) {
    if (len(list) == 0) {
	  return acc 
	}
	else {
	  let head = first(list)
	  let tail = rest(list)
	  let accumulated = push(acc, f(head))
	  iter(tail, accumulated)
	}
  }
  iter(list,[])
}
let add_one = fn(x) { x + 1 };
puts(map([1,2,3],add_one))
// [2,3,4]

```

## Known bugs

```js
let array = []
let array = push(array,10)
// Error empty item
// Should be [10]
```


See the [open issues](https://github.com/k-kahora/writing-an-interpreter-in-ocaml/issues?q=is%3Aopen) for a full list of proposed features (and known issues).


<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- ROADMAP -->
## TODO

- [x] Allow CLI to either interpret or compile
- [x] Implement closures from the last chapter of the compiler book
- [x] Create a logo 
- [ ] Implement Macros in the Interpreter and Compiler
- [ ] Refreactor the lexer to no longer throw exceptions and insted output Errors
- [ ] Add documentation to be generated with ocamldoc
- [ ] File extensions for the compiler to recognize .maml
- [ ] Syntax sugar
    - [x] <- instead of = (completely remove = so equality is not ==)
    - [ ] % modulo
    - [ ] ?? !! ternary conditionals
    - [ ] support <= and >= conditionals
    - [ ] ** powers
    - [ ] ( +=/-=/%=/*= ) infix -:- and ( ++ and -- ) postfix
- [ ] Lexer should store token location to have errors point to the right spot
- [ ] Built in functions
    - [ ] dump() take a string and output the bytecode
	- [ ] map(list,f)  
	- [ ] fold(f(a,b),acc,list)  fold function on arrays and dictionarys
	- [ ] iter(list) iterative over lists and arrays
- [ ] Publish the website to run Maml in the browser

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

If you have a suggestion that would make this better, please fork the repo and create a pull request. You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE.txt` for more information.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- CONTACT -->
## Contact

Malcolm Kahora - malcolmkahora@gmail.com

Project Link: [https://github.com/k-kahora/writing-an-interpreter-in-ocaml](https://github.com/k-kahora/writing-an-interpreter-in-ocaml)

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- ACKNOWLEDGMENTS -->
## Acknowledgments

* [Choose an Open Source License](https://choosealicense.com)
* [GitHub Emoji Cheat Sheet](https://www.webpagefx.com/tools/emoji-cheat-sheet)
* [Img Shields](https://shields.io)
* [GitHub Pages](https://pages.github.com)
* [Ocaml Programming](https://cs3110.github.io/textbook/cover.html)
* [Cmdline](https://erratique.ch/software/cmdliner)
* [Writing in Intreprter/Compiler in Go](https://interpreterbook.com/)

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[forks-shield]: https://img.shields.io/github/forks/k-kahora/Maml.svg?style=for-the-badge
[forks-url]: https://github.com/k-kahora/Maml/network/members
[stars-shield]: https://img.shields.io/github/stars/k-kahora/Best-README-Template.svg?style=for-the-badge
[stars-url]: https://github.com/k-kahora/writing-an-interpreter-in-ocaml/stargazers
[issues-shield]: https://img.shields.io/github/issues/k-kahora/Best-README-Template.svg?style=for-the-badge
[issues-url]: https://github.com/k-kahora/Maml/issues
[license-shield]: https://img.shields.io/github/license/k-kahora/Best-README-Template.svg?style=for-the-badge
[license-url]: https://github.com/k-kahora/Maml/blob/master/LICENSE.txt
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://linkedin.com/in/othneildrew
[product-screenshot]: images/screenshot.png
[Next.js]: https://img.shields.io/badge/next.js-000000?style=for-the-badge&logo=nextdotjs&logoColor=white
[Next-url]: https://nextjs.org/
[React.js]: https://img.shields.io/badge/React-20232A?style=for-the-badge&logo=react&logoColor=61DAFB
[React-url]: https://reactjs.org/
[Vue.js]: https://img.shields.io/badge/Vue.js-35495E?style=for-the-badge&logo=vuedotjs&logoColor=4FC08D
[Vue-url]: https://vuejs.org/
[Angular.io]: https://img.shields.io/badge/Angular-DD0031?style=for-the-badge&logo=angular&logoColor=white
[Angular-url]: https://angular.io/
[Svelte.dev]: https://img.shields.io/badge/Svelte-4A4A55?style=for-the-badge&logo=svelte&logoColor=FF3E00
[Svelte-url]: https://svelte.dev/
[Laravel.com]: https://img.shields.io/badge/Laravel-FF2D20?style=for-the-badge&logo=laravel&logoColor=white
[Laravel-url]: https://laravel.com
[Bootstrap.com]: https://img.shields.io/badge/Bootstrap-563D7C?style=for-the-badge&logo=bootstrap&logoColor=white
[Bootstrap-url]: https://getbootstrap.com
[JQuery.com]: https://img.shields.io/badge/jQuery-0769AD?style=for-the-badge&logo=jquery&logoColor=white
[JQuery-url]: https://jquery.com 
[Ocaml.com]: https://img.shields.io/badge/Ocaml-%23?style=for-the-badge&logo=Ocaml&logoColor=white&logoSize=auto&labelColor=%23EC6813&color=%23EC6813
[Ocaml-url]: https://ocaml.com 
[Nixos-url]: https://img.shields.io/badge/Nixos-%23?style=for-the-badge&logo=Nixos&logoSize=auto&color=grey
[Nixos.com]:https://nixos.org/
