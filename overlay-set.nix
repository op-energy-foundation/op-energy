{GIT_COMMIT_HASH}:
let
  op-energy-blockspan-service-api-overlay =
    import ../op-energy-blockspan-service/op-energy-api/overlay.nix;
  op-energy-blockspan-service-overlay =
    import ../op-energy-blockspan-service/op-energy-backend/overlay.nix {
      GIT_COMMIT_HASH = GIT_COMMIT_HASH;
    };
  op-energy-account-api-overlay = import ./oe-account-service/op-energy-account-api/overlay.nix;
  op-energy-account-service-overlay = import ./oe-account-service/op-energy-account-service/overlay.nix {
    GIT_COMMIT_HASH = GIT_COMMIT_HASH;
  };
  op-energy-offer-api-overlay = import ./oe-offer-service/op-energy-offer-api/overlay.nix;
  stable = import ./nixpkgs.nix;
  pkgs = import stable {
    config = {};
    overlays = [
      op-energy-blockspan-service-api-overlay
      op-energy-blockspan-service-overlay
      op-energy-account-api-overlay
      op-energy-account-service-overlay
      op-energy-offer-api-overlay
    ];
  };
  op-energy = {
    op-energy-api = pkgs.op-energy-api;
    op-energy-backend = pkgs.op-energy-backend;
    op-energy-account-api = pkgs.op-energy-account-api;
    op-energy-account-service = pkgs.op-energy-account-service;
    op-energy-account-service-nginx-vhost-config = pkgs.op-energy-account-service-nginx-vhost-config;
    op-energy-offer-api = pkgs.op-energy-offer-api;
  };
in
op-energy // {
  op-energy = op-energy;
}
