#!/usr/bin/env bash

source "$CONFIG_DIR/colors.sh"

# Get current focused workspace
FOCUSED_WORKSPACE=$(aerospace list-workspaces --focused)

# Check all workspaces 1-9 and update them
for WORKSPACE_ID in {1..9}; do
    # Count windows in this workspace
    WINDOW_COUNT=$(aerospace list-windows --workspace "$WORKSPACE_ID" 2>/dev/null | wc -l | awk '{print $1}')
    
    # Format label as "workspace:count"
    LABEL="$WORKSPACE_ID:$WINDOW_COUNT"
    
    # Only show workspaces that have windows
    if [ "$WINDOW_COUNT" -gt 0 ]; then
        # Workspace has windows - show it
        if [ "$WORKSPACE_ID" = "$FOCUSED_WORKSPACE" ]; then
            # This is the active workspace with windows
            sketchybar --set workspace.$WORKSPACE_ID \
                label="$LABEL" \
                background.drawing=on \
                drawing=on
        else
            # Inactive workspace with windows
            sketchybar --set workspace.$WORKSPACE_ID \
                label="$LABEL" \
                label.color="$WS_ACTIVE_LABEL_COLOR" \
                background.color="$WS_WIHT_CLIENTS_BG_COLOR" \
                background.drawing=on \
                drawing=on
        fi
    else
        # No windows - hide the workspace
        sketchybar --set workspace.$WORKSPACE_ID \
            drawing=off
    fi
done
