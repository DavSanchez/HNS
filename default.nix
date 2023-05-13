{ mkDerivation, base, lib }:
mkDerivation {
  pname = "HNS";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  license = lib.licenses.gpl3Only;
  mainProgram = "HNS";
}
