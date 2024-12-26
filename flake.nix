{
  description = "Cross-platform system configuration";

  inputs = {
    # Core package sources
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # Home Manager for user environment management
    home-manager = {
      url = "github:nix-home-manager/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Darwin system configuration
    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, darwin }: {
    # Darwin configuration for macOS systems
    darwinConfigurations."logicia" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";  # For Apple Silicon, use "x86_64-darwin" for Intel
      modules = [
        ./darwin.nix
        home-manager.darwinModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.joshua = import ./home.nix;
        }
      ];
    };

    # Home Manager configuration for Linux systems
    homeConfigurations."joshua" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules = [
        ./home.nix
        ./linux.nix
      ];
    };
  };
}
