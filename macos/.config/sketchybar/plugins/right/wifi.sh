#!/usr/bin/env bash

source "$CONFIG_DIR/icons.sh"
source "$CONFIG_DIR/colors.sh"

# Icon only. macOS 15 redacts the SSID ("<redacted>") for processes without
# Location Services access, so connected/disconnected is all we can show.
INTERFACE="en0"

if [ -n "$(ipconfig getifaddr "$INTERFACE" 2>/dev/null)" ]; then
    sketchybar --set "$NAME" icon="$WIFI_CONNECTED" icon.color="$WIFI_ICON_COLOR"
else
    sketchybar --set "$NAME" icon="$WIFI_DISCONNECTED" icon.color="$WIFI_DISCONNECTED_ICON_COLOR"
fi
