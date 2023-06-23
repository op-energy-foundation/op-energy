let
  pkgs = (import <nixpkgs> {
    overlays = [ (import ./overlay.nix { GIT_COMMIT_HASH = ""; } ) ];
  }); # first, load the nixpkgs with system-wide overlays
in pkgs.op-energy