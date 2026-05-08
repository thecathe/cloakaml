{
  pkgs ? import <nixpkgs> { },
}:

# let
#   zshconfig = import /home/cathe/dots/.config/home-manager/zsh { inherit pkgs; };
# in
pkgs.mkShell {
  # strictDeps = true;
  # inputsFrom = [
  #   zshconfig
  # ];
  buildInputs = with pkgs; [
    zsh
    opam
    erlang
    rebar3
  ];
  shellHook = ''
    eval $(opam env)
    opam switch cloakaml
  '';
}
