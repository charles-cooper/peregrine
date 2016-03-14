{ pkgs ? (import <nixpkgs> {}) }:
let
  drv = pkgs.haskell.packages.lts-5_5.callPackage (import ./default.nix) {};
in
  if pkgs.lib.inNixShell then drv.env else drv
