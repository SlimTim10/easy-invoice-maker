{
  nixpkgs ? import <nixpkgs>
}:

let
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

  pkgs = nixpkgs { inherit config; };

in addRuntimeDependency pkgs.haskellPackages.easy-invoice-maker pkgs.wkhtmltopdf
