{ nixpkgs ? import <nixpkgs> {
  overlays = [
    (self: super: {
      haskellPackages = super.haskellPackages.override {
        overrides = hself: hsuper: {
          recursion-schemes = hsuper.callCabal2nix "recursion-schemes" (
            builtins.fetchTarball "https://github.com/recursion-schemes/recursion-schemes/archive/30767f798889364d87ff874c0dd46972c4ab2f40.tar.gz"
          ) {};
        };
      };
    })
  ];
} }:
let package = nixpkgs.callPackage ./. { };
in package.env
