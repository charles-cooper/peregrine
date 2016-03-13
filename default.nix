{ stdenv, pkgs, profiling ? false}:

let
  haskellEnv = pkgs.haskell.packages.lts-5_5.ghcWithPackages (p: with p; [
    interpolatedstring-perl6
    language-c-quote
  ]);
in
  stdenv.mkDerivation {
    name        = "gimmel";
    buildInputs = [
      haskellEnv
   ];
    shellHook   = ''
      echo env "${haskellEnv}"
      export PS1="\[\033[1;32m\][\h \W]\$\[\033[0m\] "
      export PS2=">"
      export PROJECT_PATH=$(pwd)
      export PATH=$PATH:$PROJECT_PATH
      export NIX_GHC="${haskellEnv}/bin/ghc"
      export NIX_GHCPKG="${haskellEnv}/bin/ghc-pkg"
      export NIX_GHC_DOCDIR="${haskellEnv}/share/doc/ghc/html"
      export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
      alias hs="runghc"
      '';
  }
