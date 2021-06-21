{ nixpkgs ? import <nixpkgs> { } }:
let package = nixpkgs.callPackage ./. { };
in package.env
