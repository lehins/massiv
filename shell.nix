let
  pkgs = import <nixpkgs> {};
  # Wrapped stack executable that uses the nix-provided GHC
  stack = pkgs.stdenv.mkDerivation {
      name = "stack-system-ghc";
      builder = pkgs.writeScript "stack" ''
        source $stdenv/setup
        mkdir -p $out/bin
        makeWrapper ${pkgs.stack}/bin/stack $out/bin/stack \
          --add-flags --system-ghc
      '';
      buildInputs = [ pkgs.makeWrapper ];
    };
in pkgs.mkShell
{
  buildInputs = [ stack pkgs.haskellPackages.ghcid pkgs.haskell.compiler.ghc901 pkgs.gmp ];

}
