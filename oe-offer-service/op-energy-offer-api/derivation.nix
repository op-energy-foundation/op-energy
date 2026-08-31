{ mkDerivation, lib, base
, hspec, text
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
    text bytestring
    lens
    op-energy-api
    op-energy-account-api
  ];
  executableHaskellDepends = [
    base
    servant-swagger swagger2
    aeson aeson-pretty
    bytestring
    op-energy-api
  ];
  enableLibraryProfiling = false;
  enableExecutableProfiling = false;
  enableSharedExecutables = false;
  enableSharedLibraries = true;
  enableLibraryForGhci = true;
  enableSeparateBinOutput = true;
  testHaskellDepends = [ base hspec text ];
  doBenchmark = false;
  doCheck = false;
  license = lib.licenses.bsd3;
}
