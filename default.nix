let
  inherit (pkgs) haskell wkhtmltopdf;
  addRuntimeDependency = drv: x: addRuntimeDependencies drv [x];
  addRuntimeDependencies = drv: xs: haskell.lib.overrideCabal drv (drv: {
    buildDepends = (drv.buildDepends or []) ++ [ pkgs.makeWrapper ];
    postInstall = ''
      ${drv.postInstall or ""}
      for exe in "$out/bin/"* ; do
        wrapProgram "$exe" --prefix PATH ":" \
          ${pkgs.lib.makeBinPath xs}
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
  };
  config.permittedInsecurePackages = [
    "qtwebkit-5.212.0-alpha4"
  ];

  pkgs = import <nixpkgs> { inherit config; };

in addRuntimeDependency pkgs.haskellPackages.easy-invoice-maker wkhtmltopdf
