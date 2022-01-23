{ pkgs ? import <nixpkgs> {}
, ocamlVersion ? import ./nix/ocamlDefaultVersion.nix }:
let
  ocamlPackages = pkgs.ocaml-ng."ocamlPackages_${ocamlVersion}";
  local = pkgs.callPackage ./. { inherit ocamlVersion; };
in
pkgs.mkShell {
  inputsFrom = with local; [ httpaf httpaf-lwt-unix httpaf-async ];
  buildInputs = [ ocamlPackages.ocaml-lsp ] ++ local.testPackages;
}
