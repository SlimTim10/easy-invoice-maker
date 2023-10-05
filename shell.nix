{ pkgs ? import <nixpkgs> {}, haskell-invoice ? (import ./.).haskell-invoice }:

pkgs.mkShell {
  name = "haskell-invoice-shell";
  buildInputs = [
    haskell-invoice
    pkgs.wkhtmltopdf  # Add "wkhtmltopdf" as a runtime dependency.
  ];
}
