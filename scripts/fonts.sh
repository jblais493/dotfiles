#!/usr/bin/env bash

# Create fonts directory if it doesn't exist
FONT_DIR="$HOME/.local/share/fonts"
mkdir -p "$FONT_DIR"
mkdir -p "/tmp/font_downloads"

# Define fonts to download - format: "name url"
FONTS=(
 "Alegreya https://fonts.google.com/download?family=Alegreya"
 "GeistMono https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/GeistMono.zip"
 "Montserrat https://fonts.google.com/download?family=Montserrat"
 "Bitter https://fonts.google.com/download?family=Bitter"
)

echo "Downloading fonts..."
for font in "${FONTS[@]}"; do
 # Split the font entry into name and URL
 name=$(echo "$font" | cut -d' ' -f1)
 url=$(echo "$font" | cut -d' ' -f2-)

 echo "Downloading $name..."
 wget -q "$url" -O "/tmp/font_downloads/$name.zip"
done

echo "Extracting fonts..."
for font_zip in /tmp/font_downloads/*.zip; do
 font_name=$(basename "$font_zip" .zip)
 echo "Extracting $font_name..."
 unzip -q "$font_zip" -d "/tmp/font_downloads/$font_name"
done

echo "Installing fonts..."
# Find and copy all TTF and OTF files
find /tmp/font_downloads -name "*.ttf" -o -name "*.otf" | while read font_file; do
 cp "$font_file" "$FONT_DIR/"
done

# Clean up
rm -rf /tmp/font_downloads

# Update font cache
echo "Updating font cache..."
fc-cache -f -v

echo "Font installation complete!"
