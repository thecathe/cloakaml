{
  pkgs ? import <nixpkgs> { },
    # overlays = [
    #   (final: prev: {
    #     zshconfig = prev.callPackage /home/cathe/dots/.config/home-manager/zsh { };
    #   })
    # ];
  # },
  # lib
}:

# pkgs.callPackage (
#   {
#     mkShell,
#     zshconfig,
#   }:
let zshconfig = import /home/cathe/dots/.config/home-manager/zsh { inherit pkgs; }; 
#     zshconfig = zshconfig_g.overrideDerivation (oldAttrs: {
#     programs.zsh.oh-my-zsh.theme = "custom";
# });
    

in
  pkgs.mkShell {
    # strictDeps = true;
    inputsFrom = [
      zshconfig
    ];
    buildInputs = with pkgs; [
      zsh
      opam
    ];
  }
# ) { }
