#!/usr/bin/env bash

# AeroSpace binding mode: hidden in 'main', a yellow "SERVICE" pill otherwise.
# Triggered by on-mode-changed in ~/.aerospace.toml.
aerospace_mode=(
    drawing=off
    icon.drawing=off
    label.color="$MODE_LABEL_COLOR"
    label.padding_left=8
    label.padding_right=8
    background.color="$MODE_BG_COLOR"
    background.corner_radius=5
    background.height=20
    script="$PLUGIN_DIR/left/aerospace_mode.sh"
)

sketchybar --add event aerospace_mode_change \
    --add item aerospace_mode left \
    --set aerospace_mode "${aerospace_mode[@]}" \
    --subscribe aerospace_mode aerospace_mode_change
