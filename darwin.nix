{ config, pkgs, ... }:

{
  # System configuration
  system = {
    defaults = {
      dock = {
        autohide = true;
        orientation = "left";
        showhidden = true;
        mru-spaces = false;
        minimize-to-application = true;
      };

      finder = {
        AppleShowAllExtensions = true;
        FXEnableExtensionChangeWarning = false;
        _FXShowPosixPathInTitle = true;
      };

      NSGlobalDomain = {
        AppleShowAllExtensions = true;
        InitialKeyRepeat = 15;
        KeyRepeat = 2;
        NSAutomaticSpellingCorrectionEnabled = false;
      };
    };

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToEscape = true;
    };
  };

  # macOS-specific programs and services
  services = {
    # Enable service management through nix-darwin
    nix-daemon.enable = true;
  };

  # Don't change this value
