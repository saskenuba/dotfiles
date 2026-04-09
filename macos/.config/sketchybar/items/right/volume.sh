#!/usr/bin/env bash

volume=(
    "${right_items_common[@]}"
    icon.color="$VOLUME_ICON_COLOR"
    icon.font.size=16
    script="$PLUGIN_DIR/right/volume.sh"
    display=1
)

sketchybar --add item volume right \
    --set volume "${volume[@]}" \
    --subscribe volume volume_change
