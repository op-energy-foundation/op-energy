{ GIT_COMMIT_HASH }:
{ pkgs
, ...
}:
{
  op-energy-offer-service = pkgs.haskellPackages.callPackage ./derivation.nix { GIT_COMMIT_HASH = GIT_COMMIT_HASH; };
  op-energy-offer-service-nginx-vhost-config = import ./nginx-vhost-config.nix;
}
