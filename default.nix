let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          haskell-invoice =
            haskellPackagesNew.callPackage ./haskell-invoice.nix { };
          
          mustache =
            haskellPackagesNew.callPackage ./nix/mustache.nix { };
        };
      };
    };
  };
  config.permittedInsecurePackages = [
    "qtwebkit-5.212.0-alpha4"
  ];

  pkgs = import <nixpkgs> { inherit config; };

in pkgs.haskellPackages.haskell-invoice
