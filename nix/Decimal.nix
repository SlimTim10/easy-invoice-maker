{ mkDerivation, base, deepseq, fetchgit, HUnit, lib, QuickCheck
, test-framework, test-framework-hunit, test-framework-quickcheck2
}:
mkDerivation {
  pname = "Decimal";
  version = "0.5.2";
  src = fetchgit {
    url = "https://github.com/PaulJohnson/Haskell-Decimal.git";
    sha256 = "1aaqrqzv36qijp85bzm300fpm4i6p0mrgs031nrai7j7wd1zns3y";
    rev = "cba1aa972fd75de6571ef3a8d1dd435291d07f1f";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base deepseq ];
  testHaskellDepends = [
    base deepseq HUnit QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2
  ];
  homepage = "https://github.com/PaulJohnson/Haskell-Decimal";
  description = "Decimal numbers with variable precision";
  license = lib.licenses.bsd3;
}
