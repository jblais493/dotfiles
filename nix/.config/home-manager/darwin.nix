{ config, pkgs, ... }:
{
  # System configuration
  system = {
    defaults = {
      # Dock configuration for maximum screen space and minimal interference
      dock = {
        autohide = true;                    # Hide dock when not in use
        autohide-delay = 0.0;               # Remove delay when showing dock
        autohide-time-modifier = 0.0;       # Make dock show/hide instantly
        orientation = "left";               # Position dock on the left like GNOME
        showhidden = true;                  # Show indicator for hidden applications
        mru-spaces = false;                 # Don't automatically rearrange spaces
        minimize-to-application = true;     # Minimize windows into application icon
        show-process-indicators = true;     # Show dots under running applications
        static-only = false;               # Show only running applications
        tilesize = 48;                     # Set reasonable icon size
      };

      # Finder configurations for better navigation
      finder = {
        AppleShowAllExtensions = true;      # Always show file extensions
        FXEnableExtensionChangeWarning = false; # Don't warn when changing extensions
        _FXShowPosixPathInTitle = true;     # Show full POSIX path in window title
        QuitMenuItem = true;                # Allow quitting Finder
        CreateDesktop = false;              # Disable desktop icons
        FXPreferredViewStyle = "Nlsv";      # List view by default
        ShowPathbar = true;                 # Show path bar
        ShowStatusBar = true;               # Show status bar
      };

      # Global system settings for speed and efficiency
      NSGlobalDomain = {
        AppleShowAllExtensions = true;
        InitialKeyRepeat = 15;              # Shortest delay until key repeat
        KeyRepeat = 2;                      # Fastest key repeat rate
        NSAutomaticSpellingCorrectionEnabled = false;
        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticDashSubstitutionEnabled = false;
        NSAutomaticPeriodSubstitutionEnabled = false;
        NSAutomaticQuoteSubstitutionEnabled = false;
        NSNavPanelExpandedStateForSaveMode = true;
        NSNavPanelExpandedStateForSaveMode2 = true;
        PMPrintingExpandedStateForPrint = true;
        PMPrintingExpandedStateForPrint2 = true;

        # Disable animations for speed
        NSWindowResizeTime = 0.001;
        NSAutomaticWindowAnimationsEnabled = false;
        NSScrollAnimationEnabled = false;
        NSWindowSupportsAutomaticInlineTitle = false;

        # Trackpad and mouse settings
        AppleEnableMouseSwipeNavigateWithScrolls = false;
        AppleEnableSwipeNavigateWithScrolls = false;
        "com.apple.swipescrolldirection" = false; # Disable natural scrolling

        # Interface settings
        AppleInterfaceStyleSwitchesAutomatically = false;
        AppleInterfaceStyle = "Dark";       # Force dark mode
      };

      # Keyboard settings
      keyboard = {
        enableKeyMapping = true;
        remapCapsLockToEscape = true;       # Vim-friendly Caps Lock behavior
      };

      # Trackpad settings for better control
      trackpad = {
        Clicking = true;                    # Enable tap to click
        TrackpadRightClick = true;         # Enable two-finger right click
      };

      # Additional system behaviors
      spaces = {
        spans-displays = false;             # Separate spaces per display
      };
    };
  };

  # System services
  services = {
    nix-daemon.enable = true;

    # Additional useful services
    yabai = {
      enable = true;                        # Window management similar to GNOME
      package = pkgs.yabai;
      config = {
        layout = "bsp";                     # Tile windows automatically
        auto_balance = "on";                # Keep windows evenly sized
        split_ratio = 0.5;
        window_gap = 8;
      };
    };
  };
}
