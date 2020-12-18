{ci ? false, haskellCompiler ? "ghc8102" }:
let
  # Import the Haskell.nix library,
  haskell-src = import ((import ./nix/sources.nix)."haskell.nix") {};
  npSrc = haskell-src.sources.nixpkgs-2009;
  npArgs = haskell-src.nixpkgsArgs;
  pin = import npSrc npArgs;

  haskell = pin.haskell-nix;

  ciOptions = [ { packages.hs-speedscope.configureFlags = [ "--ghc-option=-Werror" ]; } ];

  opts = [ { packages.vault.doHaddock = false; } ];

  # Instantiate a package set using the generated file.
  pkgSet = haskell.cabalProject {
    compiler-nix-name = haskellCompiler;
    src = haskell.haskellLib.cleanGit { name = "hs-speedscope"; src = ./.; };
    modules = (if ci then ciOptions else []) ++ opts;
    index-state = "2020-12-11T00:00:00Z";
  };

in
  { hs-speedscope = pkgSet.hs-speedscope.components.exes.hs-speedscope ; }
