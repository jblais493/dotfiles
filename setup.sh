#!/usr/bin/env bash
# This script performs a complete system setup for both Linux and macOS environments.
# It handles package management, repository setup, and configuration deployment.

# Enable strict error handling
set -euo pipefail

# Configuration variables
DOTFILES_DIR="$HOME/dotfiles"
CONFIG_DIR="$HOME/.config"
REPOS=(
    "https://github.com/jblais493/Kmonad-thinkpad:$CONFIG_DIR/kmonad"
    "https://github.com/jblais493/scripts:$CONFIG_DIR/scripts"
)

# Color output functions for better visibility
print_info() { echo -e "\033[1;34m[INFO]\033[0m $1"; }
print_success() { echo -e "\033[1;32m[SUCCESS]\033[0m $1"; }
print_error() { echo -e "\033[1;31m[ERROR]\033[0m $1" >&2; }
print_warning() { echo -e "\033[1;33m[WARNING]\033[0m $1"; }

# Enhanced error handling
handle_error() {
    print_error "$1"
    exit 1
}

# Command existence checker with detailed feedback
command_exists() {
    if ! command -v "$1" >/dev/null 2>&1; then
        print_warning "Command '$1' not found"
        return 1
    fi
    return 0
}

# OS detection for cross-platform support
detect_os() {
    case "$(uname -s)" in
        Darwin*)
            echo "macos"
            ;;
        Linux*)
            echo "linux"
            ;;
        *)
            handle_error "Unsupported operating system"
            ;;
    esac
}

# Package installation function that handles both apt and brew
install_packages() {
    local os_type=$(detect_os)
    local packages=("$@")

    if [[ "$os_type" == "macos" ]]; then
        if ! command_exists brew; then
            print_info "Installing Homebrew..."
            /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" || handle_error "Failed to install Homebrew"
        fi
        brew install "${packages[@]}" || handle_error "Failed to install packages with Homebrew"
    else
        sudo apt update || handle_error "Failed to update package lists"
        sudo apt install -y "${packages[@]}" || handle_error "Failed to install packages with apt"
    fi
}

# Enhanced repository management
setup_repositories() {
    print_info "Setting up repositories..."
    mkdir -p "$CONFIG_DIR"

    for repo in "${REPOS[@]}"; do
        IFS=':' read -r repo_url target_dir <<< "$repo"
        if [ -d "$target_dir" ]; then
            print_info "Updating repository in $target_dir..."
            (cd "$target_dir" && git pull) || handle_error "Failed to update repository: $repo_url"
        else
            print_info "Cloning repository to $target_dir..."
            git clone "$repo_url" "$target_dir" || handle_error "Failed to clone repository: $repo_url"
        fi
    done
}

# Kmonad setup with platform-specific handling
setup_kmonad() {
    local os_type=$(detect_os)
    print_info "Setting up kmonad..."

    if [[ "$os_type" == "linux" ]]; then
        # Linux-specific kmonad setup
        if ! command_exists kmonad; then
            sudo wget https://github.com/kmonad/kmonad/releases/download/0.4.3/kmonad -O /usr/bin/kmonad || handle_error "Failed to download kmonad"
            sudo chmod +x /usr/bin/kmonad || handle_error "Failed to make kmonad executable"
        fi

        # Group management
        for group in input uinput; do
            if ! getent group "$group" > /dev/null; then
                sudo groupadd "$group" || handle_error "Failed to create group: $group"
            fi
            if ! groups "$USER" | grep -q "\b$group\b"; then
                sudo usermod -aG "$group" "$USER" || handle_error "Failed to add user to group: $group"
            fi
        done

        # udev rules setup
        echo 'KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"' | \
            sudo tee /etc/udev/rules.d/90-uinput.rules > /dev/null || handle_error "Failed to create udev rule"
        echo uinput | sudo tee /etc/modules-load.d/uinput.conf > /dev/null || handle_error "Failed to configure module loading"
    elif [[ "$os_type" == "macos" ]]; then
        print_warning "Kmonad setup on macOS requires manual configuration. Please refer to the documentation."
    fi
}

# Improved dotfiles deployment
setup_dotfiles() {
    print_info "Setting up dotfiles..."

    if [ ! -d "$DOTFILES_DIR" ]; then
        handle_error "Dotfiles directory not found: $DOTFILES_DIR"
    fi

    cd "$DOTFILES_DIR" || handle_error "Failed to change to dotfiles directory"

    # Backup existing configurations
    local backup_dir="$HOME/.config-backup-$(date +%Y%m%d-%H%M%S)"
    mkdir -p "$backup_dir"

    # Stow each configuration separately to handle errors gracefully
    for dir in */; do
        dir=${dir%/}
        if [ -d "$dir" ] && [ "$dir" != "." ] && [ "$dir" != ".." ]; then
            print_info "Stowing $dir..."
            if ! stow -v "$dir" 2>/dev/null; then
                print_warning "Backing up existing $dir configuration..."
                mv "$HOME/.${dir}" "$backup_dir/" 2>/dev/null || true
                stow -v "$dir" || handle_error "Failed to stow $dir"
            fi
        fi
    done
}

# Main execution
main() {
    print_info "Starting system setup..."

    # Install required packages
    install_packages git curl wget stow

    # Run setup functions
    setup_repositories
    setup_kmonad
    setup_dotfiles

    print_success "Setup completed successfully!"
    print_warning "Please log out and log back in for group changes to take effect."
    print_warning "Some changes may require a system restart to be fully applied."
}

# Execute main function
main
