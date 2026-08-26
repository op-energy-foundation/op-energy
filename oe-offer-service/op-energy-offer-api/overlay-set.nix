{ pkgs ? {}
, ...
}:
let
in
{
  op-energy-offer-api = pkgs.haskellPackages.callPackage ./derivation.nix {};
}
