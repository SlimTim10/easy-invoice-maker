{ mkDerivation, base, lib, Decimal, mustache, safe-money, safe-money-aeson }:
mkDerivation {
  pname = "haskell-invoice";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base Decimal mustache safe-money safe-money-aeson ];
  license = lib.licenses.bsd3;
}
