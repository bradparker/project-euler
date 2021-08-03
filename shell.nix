let
  recursion-schemes-source = builtins.fetchTarball
    "https://github.com/recursion-schemes/recursion-schemes/archive/30767f798889364d87ff874c0dd46972c4ab2f40.tar.gz";
in { nixpkgs ? import <nixpkgs> {
  overlays = [
    (self: super: {
      haskell = super.haskell // {
        packageOverrides = hself: hsuper: {
          recursion-schemes =
            hsuper.callCabal2nix "recursion-schemes" recursion-schemes-source
            { };
        };
      };
    })
  ];
} }:
let package = nixpkgs.callPackage ./. { };
in package.env
