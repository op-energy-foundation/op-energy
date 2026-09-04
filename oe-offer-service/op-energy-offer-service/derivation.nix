{ mkDerivation, lib, base
, hspec, text
, servant, servant-server, servant-client, servant-client-core, servant-swagger, swagger2
, http-client, http-types
, aeson
, bytestring
, containers
, lens
, warp
, persistent, persistent-template, persistent-postgresql, monad-logger
, resource-pool
, async
, exceptions
, op-energy-api
, op-energy-account-api
, op-energy-offer-api
, stm
, transformers
, prometheus-client
, prometheus-metrics-ghc
, prometheus-proc
, wai-middleware-prometheus
, safe-exceptions
, unliftio-core
, resourcet
, mtl
, GIT_COMMIT_HASH
, ...
}:
mkDerivation {
  pname = "op-energy-offer-service";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    op-energy-api
    op-energy-account-api
    op-energy-offer-api
    servant servant-server servant-client servant-client-core servant-swagger swagger2
    http-client http-types
    aeson
    text bytestring
    containers
    lens
    persistent persistent-template persistent-postgresql monad-logger
    resource-pool
    exceptions
    safe-exceptions
    mtl
    stm
    transformers
    warp
    monad-logger
    prometheus-client
    prometheus-metrics-ghc
    prometheus-proc
    wai-middleware-prometheus
    unliftio-core
    resourcet
  ];
  preBuild = ''
    sed -i 's/GIT_COMMIT_HASH/${GIT_COMMIT_HASH}/' src/OpEnergy/Offer/Server/GitCommitHash.hs
  '';
  executableHaskellDepends = [ base warp async ];
  enableLibraryProfiling = false;
  enableExecutableProfiling = false;
  enableSharedExecutables = false;
  enableSharedLibraries = false;
  enableLibraryForGhci = false;
  enableSeparateBinOutput = false;
  testHaskellDepends = [ base hspec text ];
  doBenchmark = false;
  doCheck = false;
  license = lib.licenses.bsd3;
}
