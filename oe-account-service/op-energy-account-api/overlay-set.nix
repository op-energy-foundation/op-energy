{ pkgs ? {}
, ...
}:
let
in
{
  op-energy-account-api = pkgs.haskellPackages.callPackage ./derivation.nix {};
}
