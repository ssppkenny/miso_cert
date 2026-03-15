# Nix shell for GHCJS development
{ pkgs ? import <nixpkgs> {} }:

let
  # Use reflex-platform which provides GHCJS
  reflex-platform = builtins.fetchGit {
    url = "https://github.com/reflex-frp/reflex-platform";
    ref = "develop";
  };

  nixpkgs = (import reflex-platform {}).nixpkgs;

in nixpkgs.mkShell {
  buildInputs = with nixpkgs; [
    haskell.compiler.ghcjs
    nodejs
    git
  ];

  shellHook = ''
    echo "GHCJS environment loaded"
    echo "GHCJS version:"
    ghcjs --version || echo "GHCJS not ready yet"
  '';
}

