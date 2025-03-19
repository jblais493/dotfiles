#!/bin/bash

# GNOME Settings Restoration Script
# This script restores GNOME settings, keybindings, and extensions
# from your dotfiles repository.

# Define source directory
SOURCE_DIR=~/dotfiles/gnome
TIMESTAMP=$(date +%Y%m%d-%H%M%S)
LOG_FILE="$SOURCE_DIR/logs/restore-$TIMESTAMP.log"

# Function to log messages
log() {
    echo "$(date +"%Y-%m-%d %H:%M:%S") - $1" | tee -a "$LOG_FILE"
}

# Check if source directory exists
if [ ! -d "$SOURCE_DIR" ]; then
    echo "Error: Source directory $SOURCE_DIR does not exist!"
    exit 1
fi

mkdir -p "$SOURCE_DIR/logs"
log "Starting GNOME settings restoration from $SOURCE_DIR..."

# Determine the Linux distribution
if [ -f /etc/os-release ]; then
    . /etc/os-release
    DISTRO=$ID
elif [ -f /etc/lsb-release ]; then
    . /etc/lsb-release
    DISTRO=$DISTRIB_ID
else
    DISTRO=$(uname -s)
fi

log "Detected distribution: $DISTRO"

# Install necessary GNOME packages if not already installed
log "Checking and installing required GNOME packages..."

install_packages() {
    local packages_to_install=()

    for pkg in "$@"; do
        if ! dpkg-query -W -f='${Status}' "$pkg" 2>/dev/null | grep -q "install ok installed"; then
            packages_to_install+=("$pkg")
        fi
    done

    if [ ${#packages_to_install[@]} -gt 0 ]; then
        log "Installing packages: ${packages_to_install[*]}"
        sudo apt-get update
        sudo apt-get install -y "${packages_to_install[@]}"
    else
        log "All required packages are already installed."
    fi
}

install_packages_fedora() {
    local packages_to_install=()

    for pkg in "$@"; do
        if ! rpm -q "$pkg" &>/dev/null; then
            packages_to_install+=("$pkg")
        fi
    done

    if [ ${#packages_to_install[@]} -gt 0 ]; then
        log "Installing packages: ${packages_to_install[*]}"
        sudo dnf install -y "${packages_to_install[@]}"
    else
        log "All required packages are already installed."
    fi
}

# Install necessary packages based on distribution
case $DISTRO in
    ubuntu|debian|pop|elementary|mint|zorin)
        log "Installing GNOME packages for Debian/Ubuntu-based system..."
        install_packages gnome-shell gnome-tweaks gnome-shell-extensions dconf-cli
        ;;
    fedora)
        log "Installing GNOME packages for Fedora..."
        install_packages_fedora gnome-shell gnome-tweaks gnome-shell-extensions dconf
        ;;
    arch|manjaro|endeavouros)
        log "Installing GNOME packages for Arch-based system..."
        if ! pacman -Qs gnome-shell > /dev/null; then
            sudo pacman -Sy --noconfirm gnome-shell
        fi
        if ! pacman -Qs gnome-tweaks > /dev/null; then
            sudo pacman -Sy --noconfirm gnome-tweaks
        fi
        if ! pacman -Qs gnome-shell-extensions > /dev/null; then
            sudo pacman -Sy --noconfirm gnome-shell-extensions
        fi
        ;;
    *)
        log "Unknown distribution. Please install GNOME Shell, GNOME Tweaks, and GNOME Shell Extensions manually."
        echo "Please make sure you have GNOME Shell, GNOME Tweaks, and GNOME Shell Extensions installed before continuing."
        read -p "Press Enter to continue or Ctrl+C to cancel..."
        ;;
esac

# Install specific extensions if listed
if [ -f "$SOURCE_DIR/extensions/installed-extensions.txt" ]; then
    log "Checking and installing required GNOME extensions..."

    # Check for gnome-extensions-cli for easier installation
    if command -v gnome-extensions-cli &> /dev/null; then
        log "Using gnome-extensions-cli for extension installation..."

        while IFS= read -r ext_id || [[ -n "$ext_id" ]]; do
            # Skip empty or commented lines
            [[ -z "$ext_id" || "$ext_id" =~ ^# ]] && continue

            # Strip any whitespace
            ext_id=$(echo "$ext_id" | xargs)

            log "Installing extension: $ext_id"
            gnome-extensions-cli install "$ext_id" || log "Failed to install extension: $ext_id"
        done < "$SOURCE_DIR/extensions/installed-extensions.txt"
    else
        log "You may need to manually install some extensions."
        log "Extensions list is available at: $SOURCE_DIR/extensions/installed-extensions.txt"

        # For Ubuntu/Debian, try using apt for common extensions
        if [[ "$DISTRO" == "ubuntu" || "$DISTRO" == "debian" || "$DISTRO" == "pop" ]]; then
            log "Attempting to install some common extensions via apt..."
            install_packages gnome-shell-extension-manager
        fi

        # Remind user to check extensions.gnome.org
        echo "For other extensions, please visit https://extensions.gnome.org"
        echo "or use the Extensions app to install the extensions listed in:"
        echo "$SOURCE_DIR/extensions/installed-extensions.txt"
    fi
fi

# Restore extensions from backup
log "Restoring extensions..."
if [ -f "$SOURCE_DIR/extensions/extensions-backup.tar.gz" ]; then
    mkdir -p "$HOME/.local/share/gnome-shell/extensions"
    tar -xzf "$SOURCE_DIR/extensions/extensions-backup.tar.gz" -C "$HOME/.local/share/gnome-shell"
    log "Extensions restored from archive."
elif [ -d "$SOURCE_DIR/extensions/individual" ] && [ "$(ls -A "$SOURCE_DIR/extensions/individual")" ]; then
    mkdir -p "$HOME/.local/share/gnome-shell/extensions"
    cp -r "$SOURCE_DIR/extensions/individual/"* "$HOME/.local/share/gnome-shell/extensions/"
    log "Extensions restored from individual backup."
else
    log "Warning: No extension backups found."
fi

# Restore settings in a specific order for better compatibility
log "Restoring GNOME settings..."

# First, restore specific settings categories
if [ -f "$SOURCE_DIR/dconf/desktop-settings.dconf" ]; then
    log "Restoring desktop settings..."
    dconf load /org/gnome/desktop/ < "$SOURCE_DIR/dconf/desktop-settings.dconf"
fi

if [ -f "$SOURCE_DIR/dconf/mutter-settings.dconf" ]; then
    log "Restoring mutter settings..."
    dconf load /org/gnome/mutter/ < "$SOURCE_DIR/dconf/mutter-settings.dconf"
fi

if [ -f "$SOURCE_DIR/dconf/shell-settings.dconf" ]; then
    log "Restoring shell settings..."
    dconf load /org/gnome/shell/ < "$SOURCE_DIR/dconf/shell-settings.dconf"
fi

if [ -f "$SOURCE_DIR/dconf/keybindings.dconf" ]; then
    log "Restoring keybindings..."
    dconf load /org/gnome/settings-daemon/plugins/media-keys/ < "$SOURCE_DIR/dconf/keybindings.dconf"
fi

if [ -f "$SOURCE_DIR/dconf/extension-settings.dconf" ]; then
    log "Restoring extension settings..."
    dconf load /org/gnome/shell/extensions/ < "$SOURCE_DIR/dconf/extension-settings.dconf"
fi

# Last, apply all settings if needed for any missed configurations
if [ -f "$SOURCE_DIR/dconf/all-settings.dconf" ]; then
    log "Would you like to restore all dconf settings? This might override some system-specific settings."
    log "Recommended only if specific settings restoration above didn't work properly."
    read -p "Restore all settings? (y/N): " restore_all
    if [[ "$restore_all" == "y" || "$restore_all" == "Y" ]]; then
        log "Restoring all dconf settings..."
        dconf load / < "$SOURCE_DIR/dconf/all-settings.dconf"
    else
        log "Skipping full dconf restoration."
    fi
fi

log "Settings restoration completed!"

# Make sure extensions are properly loaded
log "Restarting GNOME Shell to apply changes..."
if [ -n "$DISPLAY" ]; then
    if command -v gnome-shell &> /dev/null; then
        # Only try to restart if we're in X11, not Wayland
        if [ "$XDG_SESSION_TYPE" != "wayland" ]; then
            busctl --user call org.gnome.Shell /org/gnome/Shell org.gnome.Shell Eval s 'Meta.restart("Restarting…")' || \
            log "Could not restart GNOME Shell automatically. Please use Alt+F2, r, Enter manually."
        else
            log "Running in Wayland session. Please log out and log back in for changes to take effect."
        fi
    fi
else
    log "Not running in a graphical session. Please log in to GNOME for changes to take effect."
fi

# Record restoration event
echo "Restoration completed on: $(date)" >> "$SOURCE_DIR/logs/restoration-history.txt"
echo "GNOME Version: $(gnome-shell --version 2>/dev/null || echo 'Unknown')" >> "$SOURCE_DIR/logs/restoration-history.txt"
echo "OS Version: $(lsb_release -ds 2>/dev/null || cat /etc/*release 2>/dev/null | head -n1 || uname -om)" >> "$SOURCE_DIR/logs/restoration-history.txt"
echo "----------------------------------------" >> "$SOURCE_DIR/logs/restoration-history.txt"

log "GNOME settings restoration completed successfully!"
echo "You may need to log out and log back in for all changes to take effect."
echo "If some extensions aren't working, please check the Extensions app or visit https://extensions.gnome.org"
