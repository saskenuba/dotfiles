#!/bin/bash

wifi=(
    "${right_items_common[@]}"
    update_freq=30
    icon="$WIFI_CONNECTED"
    icon.color="$WIFI_ICON_COLOR"
    icon.y_offset=2
    icon.font.size=16
    label.color="$GREEN"
    label.font="$FONT:Regular:14.0"
    script="$PLUGIN_DIR/right/wifi.sh"
    click_script="open \"x-apple.systempreferences:com.apple.Network-Settings.extension\""
    display=1
)

sketchybar --add item wifi right \
    --set wifi "${wifi[@]}" \
    --subscribe wifi wifi_change system_woke
