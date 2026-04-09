#!/usr/bin/env bash

source "$CONFIG_DIR/colors.sh"

# Active workspace colors
LABEL_ACTIVE="${WS_ACTIVE_LABEL_COLOR:-$GREEN}"
LABEL_INACTIVE="${WS_INACTIVE_LABEL_COLOR:-$RED}"
BG_ACTIVE="${WS_ACTIVE_BG_COLOR:-0xff44475a}"
BG_INACTIVE="${WS_INACTIVE_BG_COLOR:-0xff333333}"

# Fetch state once for all workspaces (2 calls total, not 18/sec)
FOCUSED=$(aerospace list-workspaces --focused 2>/dev/null)

# Build a single batched sketchybar command for all 9 workspaces
args=()

for i in {1..9}; do
    WINDOW_COUNT=$(aerospace list-windows --workspace "$i" 2>/dev/null | wc -l | tr -d ' ')

    if [ "$i" = "$FOCUSED" ]; then
        args+=(--set workspace."$i"
            label="$i:$WINDOW_COUNT"
            label.color="$LABEL_ACTIVE"
            background.color="$BG_ACTIVE"
            background.drawing=on
            drawing=on)
    elif [ "$WINDOW_COUNT" -gt 0 ]; then
        args+=(--set workspace."$i"
            label="$i:$WINDOW_COUNT"
            label.color="$LABEL_INACTIVE"
            background.color="$BG_INACTIVE"
            background.drawing=on
            drawing=on)
    else
        args+=(--set workspace."$i"
            drawing=off)
    fi
done

# Single IPC call to sketchybar instead of 9 separate ones
sketchybar "${args[@]}"
