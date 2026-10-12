#!/bin/bash

wifi=(
    "${right_items_common[@]}"
    update_freq=30
    icon="$WIFI_CONNECTED"
    icon.color="$WIFI_ICON_COLOR"
    icon.y_offset=2
    icon.font.size=16
    label.drawing=off
    popup.height=30
    script="$PLUGIN_DIR/right/wifi.sh"
    display=1
)

wifi_popup_item=(
    icon.padding_right=6
    background.padding_right=10
    background.padding_left=10
    background.drawing=off
    icon.color="$APPLE_POPUP_COLOR"
    label.color="$APPLE_POPUP_COLOR"
)

# Clicks go through the script (mouse.clicked) so it can tell left from right:
#   left  -> Network Settings
#   right -> popup (router admin page, Network Settings)
sketchybar --add item wifi right \
    --set wifi "${wifi[@]}" \
    --subscribe wifi wifi_change system_woke mouse.clicked mouse.exited.global \
    \
    --add item wifi.router popup.wifi \
    --set wifi.router "${wifi_popup_item[@]}" \
    icon="$ROUTER" \
    label="Router" \
    click_script="sketchybar --set wifi popup.drawing=off; open \"http://\$(ipconfig getoption en0 router)\"" \
    \
    --add item wifi.settings popup.wifi \
    --set wifi.settings "${wifi_popup_item[@]}" \
    icon="$PREFERENCES" \
    label="Network Settings" \
    click_script="sketchybar --set wifi popup.drawing=off; open \"x-apple.systempreferences:com.apple.Network-Settings.extension\""
