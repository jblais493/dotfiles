#!/bin/bash
# Save as ~/Dotfiles/nixos/apply-config.sh

# Copy configs to system location
sudo cp -r ~/Dotfiles/nixos/* /etc/nixos/

# Optionally rebuild
read -p "Rebuild NixOS now? (y/n): " answer
if [[ $answer == "y" ]]; then
    sudo nixos-rebuild switch
fi
