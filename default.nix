{ mkDerivation, attoparsec, base, base16-bytestring, bytestring
, lib, network, random
}:
mkDerivation {
  pname = "HNS";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base base16-bytestring bytestring network random
  ];
  license = lib.licenses.gpl3Only;
  mainProgram = "HNS";
}
