{ mkDerivation
, base
, lib
, Decimal
, mustache
, safe-money
, safe-money-aeson
, smtp-mail
, mime-mail
, bytestring
, time
, writeShellApplication
, wkhtmltopdf
}:

let
  easy-invoice-maker = mkDerivation {
    pname = "easy-invoice-maker";
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
      bytestring
      time
    ];
    license = lib.licenses.bsd3;
  };

in
writeShellApplication {
  name = "easy-invoice-maker";
  runtimeInputs = [
    easy-invoice-maker
    wkhtmltopdf
  ];

  text = ''
    easy-invoice-maker "$@"
  '';
}
