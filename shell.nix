{ pkgs ? (import <nixpkgs> {}) }:

(import ./default.nix) {
  stdenv  = pkgs.stdenv;
  pkgs    = pkgs;
}
