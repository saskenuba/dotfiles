#!/usr/bin/env bash

# Source base configuration
source "$CONFIG_DIR/colors.sh"

# ============================================
# WORKSPACE VISUAL CONFIGURATION
# ============================================
LABEL_FONT="${LABEL_FONT:-SF Pro:Regular:14.0}"
LABEL_PADDING_LEFT=8
LABEL_PADDING_RIGHT=8
BG_CORNER_RADIUS=5
BG_HEIGHT=20
BG_PADDING_LEFT=8
BG_PADDING_RIGHT=8
ITEM_PADDING_LEFT=3
ITEM_PADDING_RIGHT=3

# Aesthetic configuration for workspace items (no script, no update_freq)
workspace_aesthetics=(
    label.font="$LABEL_FONT"
    label.color="$WS_DEFAULT_COLOR"
    label.padding_left=$LABEL_PADDING_LEFT
    label.padding_right=$LABEL_PADDING_RIGHT
    icon.drawing=off
    background.corner_radius=$BG_CORNER_RADIUS
    background.height=$BG_HEIGHT
    background.drawing=off
    background.padding_left=$BG_PADDING_LEFT
    background.padding_right=$BG_PADDING_RIGHT
    padding_left=$ITEM_PADDING_LEFT
    padding_right=$ITEM_PADDING_RIGHT
    drawing=off
)

# Create workspace items 1-9 (display-only, no scripts)
for i in {1..9}; do
    sketchybar --add item workspace.$i left \
        --set workspace.$i \
        "${workspace_aesthetics[@]}" \
        label="$i:0" \
        click_script="aerospace workspace $i"
done

# Single controller item: drives all workspace updates via one script
sketchybar --add event aerospace_workspace_change \
    --add item workspace_controller left \
    --set workspace_controller \
        drawing=off \
        update_freq=3 \
        script="$PLUGIN_DIR/left/aerospace_workspaces.sh" \
    --subscribe workspace_controller aerospace_workspace_change
