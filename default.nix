let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          easy-invoice-maker =
            haskellPackagesNew.callPackage ./easy-invoice-maker.nix { };
          
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

in pkgs.haskellPackages.easy-invoice-maker
