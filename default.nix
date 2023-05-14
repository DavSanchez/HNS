{ mkDerivation, base, base16-bytestring, bytestring, lib
, megaparsec, network, random
}:
mkDerivation {
  pname = "HNS";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base base16-bytestring bytestring megaparsec network random
  ];
  license = lib.licenses.gpl3Only;
  mainProgram = "HNS";
}
