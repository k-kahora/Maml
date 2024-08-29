{
  description = "o-caml flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        legacyPackages = nixpkgs.legacyPackages.${system};
        ocamlPackages = legacyPackages.ocamlPackages;
        lib = legacyPackages.lib;

        src = lib.cleanSourceWith {
          src = ./.;
          filter = name: type: lib.cleanSourceFilter name type && ! (lib.hasSuffix ".git" name);
        };

        buildOcamlPackage = { name, buildInputs }: legacyPackages.stdenv.mkDerivation {
          inherit name src;
          buildInputs = with ocamlPackages; [
            dune_3
            ocaml
            merlin
            utop
          ] ++ buildInputs;
          buildPhase = ''
            dune build @install
          '';
          installPhase = ''
            dune install --prefix=$out
          '';
        };

      in
      {
        devShells.default = legacyPackages.mkShell {
          packages = [
            legacyPackages.nixpkgs-fmt
            legacyPackages.tree-sitter
            legacyPackages.gcc
            legacyPackages.ocamlformat
            legacyPackages.fswatch
            legacyPackages.ocaml
            legacyPackages.tree-sitter-grammars.tree-sitter-ocaml
            legacyPackages.opam
            legacyPackages.rlwrap
            legacyPackages.just
            ocamlPackages.odoc
            ocamlPackages.cmdliner
            ocamlPackages.ocamlc-loc
            ocamlPackages.ocaml-lsp
            ocamlPackages.alcotest
            ocamlPackages.zarith
            ocamlPackages.ocamlformat-rpc-lib
            ocamlPackages.xxhash # Brought this in to quickly hash a string
            ocamlPackages.utop
            ocamlPackages.core
            ocamlPackages.base
            ocamlPackages.core_unix
            ocamlPackages.fmt
            ocamlPackages.dune_3
            ocamlPackages.ocaml
            ocamlPackages.merlin
            ocamlPackages.dot-merlin-reader
          ];
        };

        packages = {
          myOcamlApp = buildOcamlPackage {
            name = "my-ocaml-app";
            buildInputs = [
              ocamlPackages.xxhash
              ocamlPackages.cmdliner
              ocamlPackages.alcotest
            ]; 
          };
        };

        apps = {
          myOcamlApp = {
            type = "app";
            program = "${self.packages.${system}.myOcamlApp}/bin/Graffiti";
          };
        };
      }
    );
}
