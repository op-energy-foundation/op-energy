let
  pkgs = (import <nixpkgs> {
    overlays = [ (import ./overlay.nix { GIT_COMMIT_HASH = ""; } ) ];
  }); # first, load the nixpkgs with system-wide overlays
  lib = pkgs.lib;
  shell = pkgs.stdenv.mkDerivation {
    name = "shell";
    buildInputs = [
      pkgs.haskellPackages.ghc pkgs.haskellPackages.swagger2 pkgs.haskellPackages.servant pkgs.haskellPackages.servant-swagger
      pkgs.haskellPackages.ghci pkgs.haskellPackages.ghcid pkgs.haskellPackages.cabal-install pkgs.swagger-codegen
      pkgs.jq
      ]
      ++ pkgs.op-energy-backend.env.nativeBuildInputs
      ++ pkgs.op-energy-backend.buildInputs
#      lib.foldl' (acc: items: acc ++ items) [] (lib.mapAttrsToList (name: cfg: cfg.env.nativeBuildInputs) pkgs.op-energy) ++
#      lib.foldl' (acc: items: acc ++ items) [] (lib.mapAttrsToList (name: cfg: cfg.buildInputs) pkgs.op-energy)
    ;
  };

in shell
