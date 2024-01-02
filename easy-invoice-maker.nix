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
, text
, writeShellApplication
, wkhtmltopdf
}:

mkDerivation {
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
    text
  ];
  license = lib.licenses.bsd3;
}
