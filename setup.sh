#!/usr/bin/env sh
# This will setup a new machine with all ditfiles in their correct locations, including firefox, gnome settings, kmonad, scripts, hotkeys etc.

# Install tooling for system setup
sudo apt update
sudo apt upgrade
sudo apt install git curl wget build-essential

# Pull in scripts and kmonad repos to ~/.config

# Setup kmonad, installing the executable in /usr/bin/kmonad

# stow all dotfiles
stow *
