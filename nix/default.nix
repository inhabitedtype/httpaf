{ pkgs, lib, stdenv, ocamlVersion, opam2nix }:
let
  inherit (lib) strings;
  args = {
    inherit (pkgs.ocaml-ng."ocamlPackages_${ocamlVersion}") ocaml;
    src =
      let ignores = pkgs.lib.strings.fileContents ../.gitignore;
      in pkgs.nix-gitignore.gitignoreSourcePure ignores ../.;
  };

  opam-selection = opam2nix.build (args // {
    selection = "${./opam-selection_${ocamlVersion}.nix}";
  });

  localPackages =
    let contents = builtins.attrNames (builtins.readDir ../.);
    in builtins.filter (strings.hasSuffix ".opam") contents;

  # list of dependencies with "with-test" flag manually
  testPackageNames = [ "alcotest" ];
  testPackages =
    builtins.map (name: builtins.getAttr name opam-selection) testPackageNames;

  resolve = opam2nix.resolve (args // {
    selection = "./nix/opam-selection_${ocamlVersion}.nix";
  }) (localPackages ++ testPackageNames);


in (builtins.listToAttrs (builtins.map (fname:
  let packageName = strings.removeSuffix ".opam" fname;
  in {
    name = packageName;
    value = builtins.getAttr packageName opam-selection;
  }) localPackages)) // {
    inherit resolve opam-selection testPackages;
  }
