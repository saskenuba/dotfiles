#!/usr/bin/env bash

clock=(
    update_freq=10
    icon="$CLOCK"
    icon.color="$CLOCK_ICON_COLOR"
    icon.padding_right=4
    label.color="$CLOCK_LABEL_COLOR"
    script="$PLUGIN_DIR/clock.sh"
    click_script="open -a Clock"
)

sketchybar --add item clock right \
    --set clock "${clock[@]}"
