{ config, pkgs, ... }:

{
  # Linux-specific packages
  home.packages = with pkgs; [
    # Linux-only packages go here
    gnome-tweaks
    lm_sensors
  ];

  # Linux-specific program configurations
  programs = {
    # Example: Configure X11 settings if needed
    xsession = {
      enable = true;
      windowManager.command = "gnome-session";
    };
  };

  # Linux-specific service configurations
  services = {
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 1800;
    };
  };
}
