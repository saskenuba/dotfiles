#!/usr/bin/env bash

# ============================================
# WORKSPACE VISUAL CONFIGURATION
# ============================================
LABEL_FONT="${LABEL_FONT:-SF Pro:Regular:14.0}"
APP_ICON_FONT="sketchybar-app-font:Regular:14.0"
LABEL_PADDING_LEFT=8
LABEL_PADDING_RIGHT=8
BG_CORNER_RADIUS=5
BG_HEIGHT=20
BG_PADDING_LEFT=8
BG_PADDING_RIGHT=8
ITEM_PADDING_LEFT=3
ITEM_PADDING_RIGHT=3

# Aesthetic configuration for workspace items (no script, no update_freq).
# icon = workspace number, label = icons of the apps in it (sketchybar-app-font)
workspace_aesthetics=(
    icon.font="$LABEL_FONT"
    icon.color="$WS_DEFAULT_COLOR"
    icon.padding_left=$LABEL_PADDING_LEFT
    icon.padding_right=$LABEL_PADDING_RIGHT
    label.font="$APP_ICON_FONT"
    label.color="$WS_DEFAULT_COLOR"
    label.padding_left=0
    label.padding_right=$LABEL_PADDING_RIGHT
    label.drawing=off
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
        icon="$i" \
        click_script="aerospace workspace $i"
done

# Single controller item: drives all workspace updates via one script.
# aerospace_workspace_change covers focus moves; space_windows_change and
# front_app_switched cover windows opening/closing (AeroSpace keeps every
# window on one macOS Space). update_freq is only a safety net.
sketchybar --add event aerospace_workspace_change \
    --add item workspace_controller left \
    --set workspace_controller \
        drawing=off \
        update_freq=30 \
        script="$PLUGIN_DIR/left/aerospace_workspaces.sh" \
    --subscribe workspace_controller aerospace_workspace_change space_windows_change front_app_switched
