with import <nixpkgs> { };

let
  hsPkgs = haskell.packages.ghc801;
in
  haskell.lib.buildStackProject {
     name = "cardano-updater";
     ghc = hsPkgs.ghc;
     buildInputs = [
       zlib openssh git bsdiff bzip2
     ] ++ (lib.optionals stdenv.isLinux [ cabal-install stack ]);
     LANG = "en_US.UTF-8";
  }
