# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";
  boot.loader.grub.useOSProber = true;

  # Setup keyfile
  boot.initrd.secrets = {
    "/boot/crypto_keyfile.bin" = null;
  };

  boot.loader.grub.enableCryptodisk = true;

  boot.initrd.luks.devices."luks-89f9b5d7-d320-4b23-8db5-e3e5823e0578".keyFile = "/boot/crypto_keyfile.bin";
  networking.hostName = "king"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Edmonton";

  # Setup Kmonad
  # Load the uinput module
  boot.kernelModules = [ "uinput" ];

  # Give permissions to /dev/uinput
  services.udev.extraRules = ''
    # KMonad user access to /dev/uinput
    KERNEL=="uinput", MODE="0660", GROUP="input", TAG+="uaccess"
  '';

  # Select internationalisation properties.
  i18n.defaultLocale = "en_CA.UTF-8";

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.joshua = {
    isNormalUser = true;
    description = "joshua";
    extraGroups = [ "networkmanager" "wheel" "uinput" "input" ];
    packages = with pkgs; [
    #  thunderbird
    ];
  };

  # Install firefox.
  programs.firefox.enable = true;

  # Hyprland setup
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;  # For X11 app support
    wrapperFeatures.gtk = true;  # For proper GTK app handling
  };

  # Setup GPG key support
  programs.gnupg.agent = {
    enable = true;
  };

  # Set zsh as default shell
  programs.zsh.enable = true;
  users.users.joshua = {
    shell = pkgs.zsh;
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # Core Utilities
    wget
	  stow
	  git
	  curl
    gcc
    clang
    glibc
    killall
    zip
    unzip
    bluez
    blueman
    bluez-tools
    libnotify
    pipewire
    wireplumber
    pavucontrol
    xdg-utils
    brightnessctl
	  networkmanager
	  networkmanagerapplet
	  coreutils
	  gnumake

    # Emacs
	  emacs
	  texliveFull

    # shell and terminal
	  kitty
    fzf
    trash-cli
    tldr
    neovim
	  btop
	  kmonad
	  eza
	  yazi
	  zoxide
    ydotool
	  pdftk
	  libreoffice
	  starship
	  zsh
	  bat
	  ripgrep-all
	  fd
	  pass
	  tmux
	  tmuxifier
	  rsync
	  gnupg
	  fastfetch
    flatpak

    # Hyprland
	  waybar
	  hyprpaper
    wofi
    wl-clipboard
    swww
    swaynotificationcenter
    grim
    slurp
    wlsunset
    polkit_gnome # for authentication flows

    # Fonts
	  geist-font
	  alegreya

    # Development
	  hugo
	  go
	  python314
	  podman
	  podman-compose

    # Graphical Applications
    xfce.thunar
	  qbittorrent
	  gimp-with-plugins
    signal-desktop
    telegram-desktop
    obs-studio
    calibre
    shotcut
    vlc
    mpv
    syncthing
    waydroid
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?

}
