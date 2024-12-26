#!/usr/bin/env bash
# This script performs a complete system setup including:
# - Installing necessary system tools
# - Setting up configuration repositories
# - Configuring kmonad for keyboard management
# - Stowing all dotfiles to their correct locations

# First, let's create a function to handle errors gracefully
handle_error() {
    echo "Error: $1"
    exit 1
}

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

echo "Starting system setup..."

# System Update and Tool Installation
echo "Updating system and installing required tools..."
if ! sudo apt update; then
    handle_error "Failed to update package lists"
fi

if ! sudo apt upgrade -y; then
    handle_error "Failed to upgrade system packages"
fi

# Install essential tools - using a single apt install command is more efficient
echo "Installing essential tools..."
if ! sudo apt install -y git curl wget build-essential stow; then
    handle_error "Failed to install essential tools"
fi

# Repository Setup
echo "Setting up configuration repositories..."
CONFIG_DIR="$HOME/.config"
mkdir -p "$CONFIG_DIR"

# Function to clone or update repositories
clone_or_update_repo() {
    local repo_url="$1"
    local target_dir="$2"

    if [ -d "$target_dir" ]; then
        echo "Updating existing repository in $target_dir..."
        cd "$target_dir" || handle_error "Failed to change to $target_dir"
        git pull
        cd - || handle_error "Failed to return from $target_dir"
    else
        echo "Cloning $repo_url to $target_dir..."
        git clone "$repo_url" "$target_dir" || handle_error "Failed to clone $repo_url"
    fi
}

# Clone configuration repositories
clone_or_update_repo "https://github.com/jblais493/Kmonad-thinkpad" "$CONFIG_DIR/kmonad"
clone_or_update_repo "https://github.com/jblais493/scripts" "$CONFIG_DIR/scripts"

# Kmonad Setup
echo "Setting up kmonad..."

# Download and install kmonad binary
if ! command_exists kmonad; then
    echo "Installing kmonad..."
    if ! sudo wget https://github.com/kmonad/kmonad/releases/download/0.4.3/kmonad -O /usr/bin/kmonad; then
        handle_error "Failed to download kmonad"
    fi
    sudo chmod +x /usr/bin/kmonad || handle_error "Failed to make kmonad executable"
else
    echo "kmonad is already installed"
fi

# Configure system groups and permissions
echo "Configuring system groups and permissions..."

# Add user to input group
if ! groups "$USER" | grep -q "\binput\b"; then
    sudo usermod -aG input "$USER" || handle_error "Failed to add user to input group"
fi

# Create and configure uinput group
if ! getent group uinput > /dev/null; then
    sudo groupadd uinput || handle_error "Failed to create uinput group"
fi

if ! groups "$USER" | grep -q "\buinput\b"; then
    sudo usermod -aG uinput "$USER" || handle_error "Failed to add user to uinput group"
fi

# Configure udev rules and module loading
echo "Setting up udev rules and module loading..."
echo 'KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"' | \
    sudo tee /etc/udev/rules.d/90-uinput.rules > /dev/null || handle_error "Failed to create udev rule"

echo uinput | sudo tee /etc/modules-load.d/uinput.conf > /dev/null || \
    handle_error "Failed to configure module loading"

# Stow dotfiles
echo "Stowing dotfiles..."
cd "$HOME/.dotfiles" || handle_error "Failed to change to dotfiles directory"
stow * || handle_error "Failed to stow dotfiles"

echo "Setup completed successfully!"
echo "Important: Please log out and log back in for group changes to take effect."
echo "You may need to restart your system for all changes to be applied."
