{ config, pkgs, ... }:

{
  home.username = "joshua";
  home.homeDirectory = if pkgs.stdenv.isDarwin
    then "/Users/joshua"
    else "/home/joshua";

  # Let Home Manager manage itself
  programs.home-manager.enable = true;

  # Packages to install
  home.packages = with pkgs; [
    # Development tools
    neovim
    emacs
    rustc
    cargo
    go
    python3
    nodejs
    pnpm

    # Command line utilities
    zoxide
    fzf
    ripgrep
    fd
    bat
    eza
    starship
    kitty

    # Document handling
    zathura
    pandoc
    poppler_utils

    # Additional tools
    thunderbird
    obs-studio
    gimp
    qbittorrent
  ];

  # Program-specific configurations that work across platforms
  programs = {
    zsh = {
      enable = true;
      initExtra = ''
        eval "$(starship init zsh)"
        eval "$(zoxide init zsh)"
      '';
    };

    git = {
      enable = true;
      userName = "Joshua Bials";
      userEmail = "josh@joshblais.com";
    };

    starship = {
      enable = true;
      settings = {
        add_newline = false;
      };
    };
  };

  # Allow unfree packages if needed
  nixpkgs.config.allowUnfree = true;

  # Don't change this value
  home.stateVersion = "23.11";
}
