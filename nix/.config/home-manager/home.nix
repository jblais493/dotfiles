{ config, pkgs, ... }:
{
  home.username = "joshua";
  home.homeDirectory = if pkgs.stdenv.isDarwin
    then "/Users/joshua"
    else "/home/joshua";

  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    # Development environments and tools
    neovim
    emacs
    rustc
    cargo
    go
    python3
    nodejs
    pnpm
    cmake  # Essential for many build processes
    gcc    # Core compiler
    git    # Though you have git program config, the binary is needed
    lazygit # Terminal UI for git that works well with neovim

    # Terminal productivity tools
    zoxide   # Smarter directory navigation
    fzf      # Fuzzy finder
    ripgrep  # Modern grep replacement
    fd       # User-friendly find replacement
    bat      # Better cat with syntax highlighting
    eza      # Modern ls replacement
    starship # Shell prompt
    kitty    # Terminal emulator
    tmux     # Terminal multiplexer for session management

    # Document processing and viewing
    zathura        # PDF viewer
    pandoc         # Universal document converter
    poppler_utils  # PDF manipulation tools
    texlive.combined.scheme-medium  # TeX distribution for document preparation

    # Media and creative tools
    thunderbird  # Email client
    obs-studio   # Screen recording/streaming
    gimp         # Image editing
    qbittorrent  # Torrent client

    # System utilities
    htop      # Process viewer
    bottom    # Modern system monitor
    duf       # Disk usage viewer

    # Additional development tools
    gh        # GitHub CLI
    jq        # JSON processor
    yq        # YAML processor
  ];

  programs = {
    zsh = {
      enable = true;
      initExtra = ''
        eval "$(starship init zsh)"
        eval "$(zoxide init zsh)"

        # Add useful aliases
        alias vim="nvim"
        alias ls="eza"
        alias cat="bat"

        # Improve history
        HISTSIZE=10000
        SAVEHIST=10000
        setopt HIST_IGNORE_DUPS
        setopt HIST_FIND_NO_DUPS
      '';

      # Enable syntax highlighting and autosuggestions
      enableSyntaxHighlighting = true;
      enableAutosuggestions = true;
    };

    git = {
      enable = true;
      userName = "Joshua Bials";
      userEmail = "josh@joshblais.com";

      # Add useful git configurations
      extraConfig = {
        init.defaultBranch = "main";
        pull.rebase = true;
        push.autoSetupRemote = true;
      };
    };

    starship = {
      enable = true;
      settings = {
        add_newline = false;

        # Customize prompt segments
        golang.symbol = "Go ";
        rust.symbol = "🦀 ";
        nodejs.symbol = "⬡ ";
        python.symbol = "Py ";
      };
    };

    # Add tmux configuration
    tmux = {
      enable = true;
      shortcut = "a";  # Use Ctrl-a as prefix
      baseIndex = 1;   # Start windows and panes at 1
      mouse = true;    # Enable mouse support
    };
  };

  # Create XDG directories
  xdg = {
    enable = true;
    userDirs = {
      enable = true;
      createDirectories = true;
    };
  };

  nixpkgs.config.allowUnfree = true;
  home.stateVersion = "23.11";
}
