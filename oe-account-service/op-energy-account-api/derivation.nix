{ mkDerivation, lib, base
, hspec, text
, servant, servant-swagger, swagger2
, servant-websockets
, aeson
, bytestring
, lens
, scientific
, cryptohash-sha256, base16-bytestring
, persistent, persistent-template
, persistent-pagination
, random
, cereal
, op-energy-api
, data-default
, ...
}:
mkDerivation {
  pname = "op-energy-account-api";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = [
    base
    servant servant-swagger swagger2
    servant-websockets
    aeson
    text bytestring
    lens
    scientific
    cryptohash-sha256 base16-bytestring
    random
    persistent persistent-template
    persistent-pagination
    cereal
    op-energy-api
    data-default
  ];
  executableHaskellDepends = [
    base
    servant-swagger swagger2
    aeson
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
