{ pkgs ? import <nixpkgs> { }}:

pkgs.mkShell {
  buildInputs = with pkgs; [
    zsh
    opam
  ];
}