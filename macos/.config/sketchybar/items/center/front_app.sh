#!/usr/bin/env bash

front_app=(
    "${center_items_common[@]}"
    icon.drawing=off
    label.color="$FRONT_APP_LABEL_COLOR"
    label.font="$LABEL_FONT"
    associated_display=active
    script="$PLUGIN_DIR/front_app.sh"
)

# 'q' = just left of the MacBook notch; 'center' would sit underneath it
sketchybar --add item front_app q \
    --set front_app "${front_app[@]}" \
    --subscribe front_app front_app_switched
