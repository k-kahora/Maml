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
    <img src="images/maml.png" alt="Logo" width="80" height="80">
  </a>

  <h3 align="center">Best-README-Template</h3>

  <p align="center">
    An awesome README template to jumpstart your projects!
    <br />
    <a href="https://github.com/k-kahora/Graffiti"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/k-kahora/Graffiti">View Demo</a>
    ·
    <a href="https://github.com/k-kahora/Graffiti/issues/new?labels=bug&template=bug-report---.md">Report Bug</a>
    ·
    <a href="https://github.com/k-kahora/Graffiti/issues/new?labels=enhancement&template=feature-request---.md">Request Feature</a>
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

[![Product Name Screen Shot][product-screenshot]](https://example.com)

A toy scripting language called graffiti based of the Monkey Programming Language -- Written in Ocaml

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

To test drive Graffiti and jump straight into the repl
* npm
  ```sh
  nix run github:k-kahora/writing-an-interpreter-in-ocaml/#myOcamlApp -- -r
  ```
  
  
* To see what graffiti can do
  ```sh
  nix run github:k-kahora/writing-an-interpreter-in-ocaml/#myOcamlApp -- --help
  ```
  

### Installation from source

When ready refrence [this](https://unix.stackexchange.com/questions/717168/how-to-package-my-software-in-nix-or-write-my-own-package-derivation-for-nixpkgs) to add my package to nixpkgs 



1. Clone the repo
   ```sh
   git clone https://github.com/k-kahora/writing-an-interpreter-in-ocaml
   ```
3. Install opam packages 
   ```sh
   opam install xxhash
   opam install cmdliner
   opam install alcotest
   ```
4. build the language
   ```sh
   dune build
   ```
5. Run the language
   ```sh
   dune exec -- Graffiti --help
   ```

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- USAGE EXAMPLES -->
## Usage
### feautures

| Feature               | Interpreter | Compiler |
|-----------------------|-------------|----------|
| Local Bindings        | ✅          | ✅       |
| Gloabl Bindings       | ✅          | ✅       |
| conditionals          | ✅          | ✅       |
| Strings               | ✅          | ✅       |
| Integers                  | ✅          | ✅       |
| Floats                | ❌          | ❌       |
| arithmatic +,-,/,*    | ✅          | ✅       |
| Arrays                | ✅          | ✅       |
| Indexing              | ✅          | ✅       |
| Dictionarys           | ✅          | ✅       |
| functions             | ✅          | ✅       |
| first class functions | ✅          | ✅       |
| Closures              | ✅          | ❌       |
| Recursion             | ✅          | ❌       |
| BuiltInFunctions      | ✅          | ✅       |
| Loops                 | ❌          | ❌       |
### small examples

These are small impractical examples curated to showcase the syntax of the language more comprehensive exapmles can be found in the documantation

Bindings
```js
let x <- 30
```


Arithmatic
```js
let x <- 30
let y <- 20
x + y
```


Conditionals
```js
let value <- 50
let result <- if (true) {value / 5} else {value / 2} 
```

Arrays
```js
let value <- [1,"hello",3,"world",5,[2,4,6],7,5,9]
let result_one <- value[1]
let result_two <- value[1 + 4][0]
```

Dictionarys
```js
let value <- {"monkey":{"see":{"monkey":{"do":"!"}}}}
let result = value["monkey"]["see"]["monkey"]["do"]
puts("monkey" + result)
```



Functions
```js
let sum <- fn(x,y) {x + y}
let result = sum(10,20)
let subtract_thirty = fn(item) {let thirty = 30; return item - 30}
let nested_square = fn() { fn(y) {  y * y } }
puts(nested_square()(10))
```

In the closure section here is the code





_For more examples, please refer to the [Documentation](https://example.com)_

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- ROADMAP -->
## Roadmap

- [x] Allow cli to either interpret or compile
- [x] Implement clojures from the last chapter of the compiler book
- [ ] Create a logo for website 
- [ ] Implement Macros in the Interpreter and Compiler
- [ ] Refreactor the lexer to no longer throw exceptions and insted output Errors
- [ ] Add documentation to be generated with ocamldoc
- [ ] File extensions for the compiler to recognize .grf
- [ ] Syntax sugar
    - [ ] <- instead of =
    - [ ] % modulo
    - [ ] ?? !! ternary conditionals
    - [ ] support <= and >= conditionals
    - [ ] ** expunation
    - [ ] ( +=/-=/%=/*= ) infix -:- and ( ++ and -- ) postfix
- [ ] Create a website similar to gleams that matches the documentation as well as examples and a way to run the code in a tutorial format

See the [open issues](https://github.com/k-kahora/writing-an-interpreter-in-ocaml/issues?q=is%3Aopen) for a full list of proposed features (and known issues).

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

Use this space to list resources you find helpful and would like to give credit to. I've included a few of my favorites to kick things off!

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
[forks-shield]: https://img.shields.io/github/forks/k-kahora/Graffiti.svg?style=for-the-badge
[forks-url]: https://github.com/k-kahora/Graffiti/network/members
[stars-shield]: https://img.shields.io/github/stars/k-kahora/Best-README-Template.svg?style=for-the-badge
[stars-url]: https://github.com/k-kahora/writing-an-interpreter-in-ocaml/stargazers
[issues-shield]: https://img.shields.io/github/issues/k-kahora/Best-README-Template.svg?style=for-the-badge
[issues-url]: https://github.com/k-kahora/Graffiti/issues
[license-shield]: https://img.shields.io/github/license/k-kahora/Best-README-Template.svg?style=for-the-badge
[license-url]: https://github.com/k-kahora/Graffiti/blob/master/LICENSE.txt
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
