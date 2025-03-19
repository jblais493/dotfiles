#!/bin/bash

# GNOME Settings Backup Script
# This script creates a comprehensive backup of GNOME settings, keybindings,
# and extensions to your dotfiles repository.

# Define backup directory
BACKUP_DIR=~/dotfiles/gnome
TIMESTAMP=$(date +%Y%m%d-%H%M%S)
LOG_FILE="$BACKUP_DIR/backup-$TIMESTAMP.log"

# Create necessary directories
mkdir -p "$BACKUP_DIR/dconf"
mkdir -p "$BACKUP_DIR/extensions"
mkdir -p "$BACKUP_DIR/logs"

echo "Starting GNOME settings backup to $BACKUP_DIR..."

# Function to log messages
log() {
    echo "$(date +"%Y-%m-%d %H:%M:%S") - $1" | tee -a "$LOG_FILE"
}

log "Creating backup directories..."

# Backup all dconf settings (contains everything including keybindings and tweaks)
log "Backing up all dconf settings..."
dconf dump / >"$BACKUP_DIR/dconf/all-settings.dconf"

# Backup specific GNOME settings categories for easier restoration
log "Backing up specific GNOME settings categories..."
dconf dump /org/gnome/ >"$BACKUP_DIR/dconf/gnome-settings.dconf"
dconf dump /org/gnome/desktop/ >"$BACKUP_DIR/dconf/desktop-settings.dconf"
dconf dump /org/gnome/shell/ >"$BACKUP_DIR/dconf/shell-settings.dconf"
dconf dump /org/gnome/settings-daemon/plugins/media-keys/ >"$BACKUP_DIR/dconf/keybindings.dconf"
dconf dump /org/gnome/shell/extensions/ >"$BACKUP_DIR/dconf/extension-settings.dconf"
dconf dump /org/gnome/mutter/ >"$BACKUP_DIR/dconf/mutter-settings.dconf"

# List installed extensions
log "Creating list of installed extensions..."
gnome-extensions list >"$BACKUP_DIR/extensions/installed-extensions.txt"

# Get details of enabled extensions
log "Determining enabled extensions..."
gsettings get org.gnome.shell enabled-extensions >"$BACKUP_DIR/extensions/enabled-extensions.txt"

# Backup extension files
log "Backing up extension files..."
if [ -d "$HOME/.local/share/gnome-shell/extensions" ]; then
    # Create a tarball of all extensions
    tar -czf "$BACKUP_DIR/extensions/extensions-backup.tar.gz" -C "$HOME/.local/share/gnome-shell" extensions

    # Also copy extensions individually for easier inspection
    mkdir -p "$BACKUP_DIR/extensions/individual"
    cp -r "$HOME/.local/share/gnome-shell/extensions/"* "$BACKUP_DIR/extensions/individual/"
    log "Extensions backed up successfully."
else
    log "Warning: Extensions directory not found. No extensions backed up."
fi

# Save system info for reference
log "Saving system information..."
echo "GNOME Version: $(gnome-shell --version)" >"$BACKUP_DIR/system-info.txt"
echo "OS Version: $(lsb_release -ds 2>/dev/null || cat /etc/*release 2>/dev/null | head -n1 || uname -om)" >>"$BACKUP_DIR/system-info.txt"
echo "Backup Date: $(date)" >>"$BACKUP_DIR/system-info.txt"

# Copy the log file to the logs directory
cp "$LOG_FILE" "$BACKUP_DIR/logs/"

log "GNOME settings backup completed successfully!"
echo "Backup stored in: $BACKUP_DIR"
