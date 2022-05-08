{ mkDerivation, aeson, base, base-unicode-symbols, bytestring
, cmdargs, containers, directory, fetchgit, filepath, hpack, hspec
, lens, lib, mtl, parsec, process, scientific, tar
, template-haskell, temporary, text, th-lift, unordered-containers
, vector, wreq, yaml, zlib
}:
mkDerivation {
  pname = "mustache";
  version = "2.4.0";
  src = fetchgit {
    url = "https://github.com/JustusAdam/mustache.git";
    sha256 = "19525f5l8fq17ar369x85vgvwj961idl7ss80bmqzb88hrgi1fmb";
    rev = "530c0f10188fdaead9688d56f728b87fabcb228b";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory filepath mtl parsec
    scientific template-haskell text th-lift unordered-containers
    vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring cmdargs filepath text yaml
  ];
  testHaskellDepends = [
    aeson base base-unicode-symbols bytestring directory filepath hspec
    lens process tar temporary text unordered-containers wreq yaml zlib
  ];
  doCheck = false;
  prePatch = "hpack";
  homepage = "https://github.com/JustusAdam/mustache";
  description = "A mustache template parser library";
  license = lib.licenses.bsd3;
}
