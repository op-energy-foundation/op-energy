{GIT_COMMIT_HASH}:
let
  op-energy-api-overlay = import ./oe-blockspan-service/op-energy-api/overlay.nix;
  op-energy-account-api-overlay = import ./oe-account-service/op-energy-account-api/overlay.nix;
  op-energy-account-service-overlay = import ./oe-account-service/op-energy-account-service/overlay.nix {
    GIT_COMMIT_HASH = GIT_COMMIT_HASH;
  };
  op-energy-backend-overlay = import ./oe-blockspan-service/op-energy-backend/overlay.nix {
    GIT_COMMIT_HASH = GIT_COMMIT_HASH;
  };
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/archive/4d2b37a84fad1091b9de401eb450aae66f1a741e.tar.gz";
  pkgs = import nixpkgs {
    config = {};
    overlays = [
      op-energy-api-overlay
      op-energy-backend-overlay
      op-energy-account-api-overlay
      op-energy-account-service-overlay
    ];
  };
  op-energy = {
    op-energy-api = pkgs.op-energy-api;
    op-energy-backend = pkgs.op-energy-backend;
    op-energy-account-api = pkgs.op-energy-account-api;
    op-energy-account-service = pkgs.op-energy-account-service;
  };
in
op-energy // {
  op-energy = op-energy;
}
