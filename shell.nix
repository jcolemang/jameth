
with import <nixpkgs> {};

haskell.lib.buildStackProject {
  name = "scheme-analysis-env";
  buildInputs = [
    cabal-install
    haskellPackages.ghc
    haskellPackages.stack
    zlib
  ];

  libraryPkgconfigDepends = [
  ];
}
