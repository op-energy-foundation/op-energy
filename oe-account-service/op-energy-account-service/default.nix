{ GIT_COMMIT_HASH ? ""
}:
let
  stable = import ../../nixpkgs.nix;
  overlay = import ../../overlay.nix { GIT_COMMIT_HASH = GIT_COMMIT_HASH;};
  pkgs = (import stable {
    overlays = [ overlay ];
  }); # first, load the nixpkgs with system-wide overlays

in pkgs.op-energy-account-service
