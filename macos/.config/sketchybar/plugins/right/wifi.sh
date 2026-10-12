#!/usr/bin/env bash

source "$CONFIG_DIR/icons.sh"
source "$CONFIG_DIR/colors.sh"

# Icon only. macOS 15 redacts the SSID ("<redacted>") for processes without
# Location Services access, so connected/disconnected is all we can show.
INTERFACE="en0"

case "$SENDER" in
    mouse.clicked)
        if [ "$BUTTON" = "right" ]; then
            # Refresh the label so it shows the current network's router
            ROUTER_IP=$(ipconfig getoption "$INTERFACE" router 2>/dev/null)
            sketchybar --set wifi.router label="Router ${ROUTER_IP:-(none)}" \
                --set "$NAME" popup.drawing=toggle
        else
            sketchybar --set "$NAME" popup.drawing=off
            open "x-apple.systempreferences:com.apple.Network-Settings.extension"
        fi
        exit 0
        ;;
    mouse.exited.global)
        sketchybar --set "$NAME" popup.drawing=off
        exit 0
        ;;
esac

if [ -n "$(ipconfig getifaddr "$INTERFACE" 2>/dev/null)" ]; then
    sketchybar --set "$NAME" icon="$WIFI_CONNECTED" icon.color="$WIFI_ICON_COLOR"
else
    sketchybar --set "$NAME" icon="$WIFI_DISCONNECTED" icon.color="$WIFI_DISCONNECTED_ICON_COLOR"
fi
