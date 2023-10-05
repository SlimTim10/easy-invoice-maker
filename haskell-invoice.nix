{ mkDerivation
, base
, lib
, Decimal
, mustache
, safe-money
, safe-money-aeson
, smtp-mail
, mime-mail
, writeShellApplication
, wkhtmltopdf
}:

let
  haskell-invoice = mkDerivation {
    pname = "haskell-invoice";
    version = "0.1.0.0";
    src = ./.;
    isLibrary = false;
    isExecutable = true;
    executableHaskellDepends =[
      base
      Decimal
      mustache
      safe-money
      safe-money-aeson
      smtp-mail
      mime-mail
    ];
    license = lib.licenses.bsd3;
  };

in
writeShellApplication {
  name = "haskell-invoice";
  runtimeInputs = [
    haskell-invoice
    wkhtmltopdf
  ];

  text = ''
    haskell-invoice "$@"
  '';
}
