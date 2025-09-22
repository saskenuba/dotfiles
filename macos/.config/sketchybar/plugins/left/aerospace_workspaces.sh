#!/usr/bin/env bash

# Arguments from sketchybar
WORKSPACE_ID="${1:-1}"
COLOR_CONFIG="${2:-}"

# Default colors if not provided
LABEL_ACTIVE_COLOR="#ffffff"
LABEL_INACTIVE_COLOR="#888888"
BG_ACTIVE_COLOR="#4c7899"
BG_INACTIVE_COLOR="#333333"

# Parse color configuration
if [ -n "$COLOR_CONFIG" ]; then
    # Parse format: "key1:value1,key2:value2,..."
    IFS=',' read -ra PAIRS <<< "$COLOR_CONFIG"
    for pair in "${PAIRS[@]}"; do
        IFS=':' read -r key value <<< "$pair"
        case "$key" in
            label_active) LABEL_ACTIVE_COLOR="$value" ;;
            label_inactive) LABEL_INACTIVE_COLOR="$value" ;;
            bg_active) BG_ACTIVE_COLOR="$value" ;;
            bg_inactive) BG_INACTIVE_COLOR="$value" ;;
        esac
    done
fi

# Get the current focused workspace
FOCUSED_WORKSPACE=$(aerospace list-workspaces --focused)

# Get window count for this workspace
WINDOW_COUNT=$(aerospace list-windows --workspace "$WORKSPACE_ID" 2>/dev/null | wc -l | tr -d ' ')

# Format the label
LABEL="$WORKSPACE_ID:$WINDOW_COUNT"

# Update based on state
if [ "$WINDOW_COUNT" -gt 0 ]; then
    # Workspace has windows - show it
    if [ "$WORKSPACE_ID" = "$FOCUSED_WORKSPACE" ]; then
        # This is the active workspace
        sketchybar --set workspace."$WORKSPACE_ID" \
            label="$LABEL" \
            label.color="$LABEL_ACTIVE_COLOR" \
            background.color="$BG_ACTIVE_COLOR" \
            background.drawing=on \
            drawing=on
    else
        # Inactive workspace with windows
        sketchybar --set workspace."$WORKSPACE_ID" \
            label="$LABEL" \
            label.color="$LABEL_INACTIVE_COLOR" \
            background.color="$BG_INACTIVE_COLOR" \
            background.drawing=on \
            drawing=on
    fi
else
    # No windows - hide the workspace
    sketchybar --set workspace."$WORKSPACE_ID" \
        drawing=off
fi