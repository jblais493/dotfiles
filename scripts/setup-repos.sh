#!/usr/bin/env bash

gpg --import ~/gpg-pub.asc
gpg --import ~/gpg-secret.asc

git clone https://github.com/jblais493/scripts ~/.config/scripts
git clone https://github.com/jblais493/Wallpapers ~/Pictures/Nord
git clone https://github.com/jblais493/Kmonad-thinkpad ~/.config/kmonad
