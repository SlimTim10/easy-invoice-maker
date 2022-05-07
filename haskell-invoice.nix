{ mkDerivation, base, lib, Decimal }:
mkDerivation {
  pname = "haskell-invoice";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base Decimal ];
  license = lib.licenses.bsd3;
}
