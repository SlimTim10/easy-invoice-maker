{
  description = "Easy Invoice Maker";
  
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        config = {
          packageOverrides = pkgs: rec {
            haskellPackages = pkgs.haskellPackages.override {
              overrides = haskellPackagesNew: haskellPackagesOld: rec {
                easy-invoice-maker =
                  haskellPackagesNew.callPackage ./easy-invoice-maker.nix {};

                mustache =
                  haskellPackagesNew.callPackage ./nix/mustache.nix {};
              };
            };
          };

          permittedInsecurePackages = [ "qtwebkit-5.212.0-alpha4" ];
        };

        pkgs = import nixpkgs {
          inherit system;
          inherit config;
        };

        addRuntimeDependency = drv: dep: addRuntimeDependencies drv [ dep ];
        addRuntimeDependencies = drv: deps: pkgs.haskell.lib.overrideCabal drv (drv: {
          buildDepends = (drv.buildDepends or []) ++ [ pkgs.makeWrapper ];
          postInstall = ''
            ${drv.postInstall or ""}
            for exe in "$out/bin/"* ; do
              wrapProgram "$exe" --prefix PATH ":" \
                ${pkgs.lib.makeBinPath deps}
            done
          '';
        });
      in {
        # nix build .#easy-invoice-maker
        packages.easy-invoice-maker =
          addRuntimeDependency pkgs.haskellPackages.easy-invoice-maker pkgs.wkhtmltopdf;

        # nix build
        defaultPackage = self.packages.${system}.easy-invoice-maker;
      }
    );
}
