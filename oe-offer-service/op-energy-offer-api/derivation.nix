{ mkDerivation, lib, base
, hspec, text, time
, servant, servant-swagger, swagger2
, aeson, aeson-pretty
, bytestring
, lens
, op-energy-api
, op-energy-account-api
, ...
}:
mkDerivation {
  pname = "op-energy-offer-api";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    servant servant-swagger swagger2
    aeson
    text bytestring time
    lens
    op-energy-api
    op-energy-account-api
  ];
  executableHaskellDepends = [
    base
    servant-swagger swagger2
    aeson aeson-pretty
    bytestring
  ];
  enableLibraryProfiling = false;
  enableExecutableProfiling = false;
  enableSharedExecutables = false;
  enableSharedLibraries = true;
  enableLibraryForGhci = true;
  enableSeparateBinOutput = true;
  testHaskellDepends = [ base hspec text ];
  doBenchmark = false;
  doHaddock = false; # we are not using it without sources, so not useful for now, but consumes CI ram during build
  doCheck = false;
  license = lib.licenses.bsd3;
}
