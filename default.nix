with import <nixpkgs> {}; {
  hawkEnv = stdenv.mkDerivation {
    name = "hawk";
    buildInputs = [
      stdenv
      llvm_35
      atom
      ghc
      cabal-install
      haskellPackages.ghc-mod
      haskellPackages.stylish-haskell
      haskellPackages.hlint
      haskellPackages.happy
      haskellPackages.alex
    ];
  };
}
