{
  description = "o-caml flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    # Do precise filtering of files in the nix store
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        legacyPackages = nixpkgs.legacyPackages.${system};
        ocamlPackages = legacyPackages.ocamlPackages;
        lib = legacyPackages.lib;
      in
      {
      devShells = {
        default = legacyPackages.mkShell {
          packages = [

            legacyPackages.nixpkgs-fmt
            legacyPackages.tree-sitter
            legacyPackages.gcc
            legacyPackages.ocamlformat
            legacyPackages.fswatch
            legacyPackages.ocaml
            legacyPackages.tree-sitter-grammars.tree-sitter-ocaml
            legacyPackages.opam
            legacyPackages.nodejs_21 # needed for tree sitter
            legacyPackages.rlwrap
            legacyPackages.just # like a makefile
            ocamlPackages.odoc
            ocamlPackages.ocamlc-loc
            ocamlPackages.ocaml-lsp
            ocamlPackages.alcotest
            ocamlPackages.zarith # Big ints
            ocamlPackages.ocamlformat-rpc-lib
            ocamlPackages.utop
            # ocamlPackages.angstrom
            ocamlPackages.core
            # ocamlPackages.re
            ocamlPackages.fmt
            # ocamlPackages.batteries
            ocamlPackages.dune_3
            ocamlPackages.ocaml
            ocamlPackages.merlin
            ocamlPackages.dot-merlin-reader # merlin needs this to read .merlin file

          ];
        };

       };
     }
    );
}
