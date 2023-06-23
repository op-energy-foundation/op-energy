let
  pkgs0 = (import <nixpkgs> {}); # first, load the nixpkgs with system-wide overlays
  pkgs = pkgs0 // (import ./../../overlay-set.nix { GIT_COMMIT_HASH = ""; }); # second, add local packages into the scope of pkgs
  shell = pkgs.stdenv.mkDerivation {
    name = "shell";
    buildInputs
      = pkgs.op-energy-account-api.env.nativeBuildInputs
      ++ pkgs.op-energy-account-api.buildInputs
      ++ [ pkgs.haskellPackages.swagger2 pkgs.haskellPackages.servant pkgs.haskellPackages.servant-swagger
         ]
      ++ [ pkgs.haskellPackages.ghci pkgs.haskellPackages.ghcid pkgs.haskellPackages.cabal-install pkgs.swagger-codegen ]
      ++ [
        pkgs.jq
      ];
  };

in shell
