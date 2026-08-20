{ GIT_COMMIT_HASH }:
{ pkgs
, ...
}:
{
  op-energy-account-service = pkgs.haskellPackages.callPackage ./derivation.nix { GIT_COMMIT_HASH = GIT_COMMIT_HASH; };
  op-energy-account-service-nginx-vhost-config = import ./nginx-vhost-config.nix;
}
