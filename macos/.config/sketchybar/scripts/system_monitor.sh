#!/usr/bin/env bash
# Click handler for the cpu/ram items; $1 is sketchybar's $BUTTON.
#   left click  -> bottom (btm) in a new Alacritty window
#   right click -> Activity Monitor

case "$1" in
    right) open -a "Activity Monitor" ;;
    *) nohup "$HOME/.cargo/bin/alacritty" -e "$HOME/.cargo/bin/btm" >/dev/null 2>&1 & ;;
esac
