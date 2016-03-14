{ pkgs ? (import <nixpkgs> {}) }:
let
  drv = pkgs.haskell.packages.lts.callPackage (import ./default.nix) {};
in
  if pkgs.lib.inNixShell then drv.env else drv
