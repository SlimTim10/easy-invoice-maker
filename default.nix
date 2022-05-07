let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          haskell-invoice =
            haskellPackagesNew.callPackage ./haskell-invoice.nix { };
          
          decimal =
            haskellPackagesNew.callPackage ./nix/Decimal.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { haskell-invoice = pkgs.haskellPackages.haskell-invoice;
  }
