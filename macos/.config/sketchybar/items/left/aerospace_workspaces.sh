#!/usr/bin/env bash

# Source base configuration
source "$CONFIG_DIR/colors.sh"

# ============================================
# WORKSPACE COLOR CONFIGURATION
# ============================================
# Active workspace colors
WS_ACTIVE_LABEL_COLOR="${GREEN:-#ffffff}"
WS_ACTIVE_BG_COLOR="${WS_ACTIVE_BG_COLOR:-#4c7899}"

# Inactive workspace with windows colors
WS_INACTIVE_LABEL_COLOR="${RED:-#888888}"
WS_INACTIVE_BG_COLOR="${WS_WITH_CLIENTS_BG_COLOR:-#333333}"

# Default workspace color (no windows)
WS_DEFAULT_COLOR="${WS_DEFAULT_COLOR:-#666666}"

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

# ============================================
# WORKSPACE BEHAVIOR CONFIGURATION
# ============================================
UPDATE_FREQUENCY=1
INITIAL_DRAWING="off"  # Set to "on" to show all workspaces initially

# ============================================
# Create color configuration string to pass to script
# Format: "key:value,key:value,..."
# ============================================
create_color_config() {
    echo "label_active:${WS_ACTIVE_LABEL_COLOR},label_inactive:${WS_INACTIVE_LABEL_COLOR},bg_active:${WS_ACTIVE_BG_COLOR},bg_inactive:${WS_INACTIVE_BG_COLOR}"
}

# Aesthetic configuration for workspace items
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
)

# Functional configuration for workspace items  
workspace_functionality=(
    drawing=$INITIAL_DRAWING
)

# Get the color configuration
COLOR_CONFIG=$(create_color_config)

# Create workspace items for 1-9
for i in {1..9}; do
    sketchybar --add item workspace.$i left \
	--add event aerospace_workspace_change \
        --set workspace.$i \
        "${workspace_aesthetics[@]}" \
        "${workspace_functionality[@]}" \
        label="$i:0" \
        script="$PLUGIN_DIR/left/aerospace_workspaces.sh $i '$COLOR_CONFIG'" \
        click_script="aerospace workspace $i" \
        --subscribe workspace.$i aerospace_workspace_change
done
