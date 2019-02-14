with { pkgs = import ./nix {}; };
pkgs.mkShell
  { buildInputs = [ pkgs.niv pkgs.haskell.compiler.ghc863 ];
  }
