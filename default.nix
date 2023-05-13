{ mkDerivation, base, bytestring, lib }:
mkDerivation {
  pname = "HNS";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base bytestring ];
  license = lib.licenses.gpl3Only;
  mainProgram = "HNS";
}
