let
  op-energy-blockspan-service-api-overlay =
    import ../../op-energy-blockspan-service/op-energy-api/overlay.nix;
  op-energy-account-api-overlay = import ../oe-account-service/op-energy-account-api/overlay.nix;
  op-energy-offer-api-overlay = import ../oe-offer-service/op-energy-offer-api/overlay.nix;
  stable = import ../nixpkgs.nix;
  pkgs = import stable {
    config = {};
    overlays = [
      op-energy-blockspan-service-api-overlay
      op-energy-account-api-overlay
      op-energy-offer-api-overlay
    ];
  };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (p: with p; [
      op-energy-api
      op-energy-account-api
      op-energy-offer-api
      servant servant-server servant-client servant-client-core servant-swagger swagger2
      http-client http-types
      aeson
      text bytestring containers
      lens
      persistent persistent-template persistent-postgresql monad-logger
      resource-pool
      exceptions safe-exceptions mtl
      stm transformers warp
      prometheus-client wai-middleware-prometheus prometheus-metrics-ghc prometheus-proc
      async unliftio-core
    ]))
    cabal-install
  ];
}
