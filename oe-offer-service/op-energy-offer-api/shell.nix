let
  op-energy-blockspan-service-api-overlay =
    import ../../op-energy-blockspan-service/op-energy-api/overlay.nix;
  op-energy-account-api-overlay = import ../oe-account-service/op-energy-account-api/overlay.nix;
  stable = import ../nixpkgs.nix;
  pkgs = import stable {
    config = {};
    overlays = [
      op-energy-blockspan-service-api-overlay
      op-energy-account-api-overlay
    ];
  };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (p: with p; [
      op-energy-api
      op-energy-account-api
      servant servant-swagger swagger2
      aeson aeson-pretty
      text bytestring
      lens
    ]))
    cabal-install
  ];
}
