#!/usr/bin/env bash

source "$CONFIG_DIR/colors.sh"
source "$CONFIG_DIR/scripts/icon_map.sh"

# Active workspace colors
LABEL_ACTIVE="$WS_ACTIVE_LABEL_COLOR"
LABEL_INACTIVE="$WS_WIHT_CLIENTS_LABEL_COLOR"
LABEL_EMPTY="$WS_EMPTY_LABEL_COLOR"
LABEL_OTHER_DISPLAY="$WS_ACTIVE_ANOTHER_DISPLAY_LABEL_COLOR"
BG_ACTIVE="$WS_ACTIVE_BG_COLOR"
BG_INACTIVE="$WS_WIHT_CLIENTS_BG_COLOR"
BG_OTHER_DISPLAY="$WS_ACTIVE_ANOTHER_DISPLAY_BG_COLOR"

# Fetch state once for all workspaces (2 calls total, not one per workspace)
# Lines look like "1|true|true": workspace|focused|visible (visible = shown on some monitor)
STATES=$(aerospace list-workspaces --all \
    --format '%{workspace}|%{workspace-is-focused}|%{workspace-is-visible}' 2>/dev/null)
# One line per (workspace, app), so an app with several windows gets one icon
PAIRS=$(aerospace list-windows --all --format '%{workspace}|%{app-name}' 2>/dev/null | sort -u)

FOCUSED=""
VISIBLE=" "
while IFS='|' read -r ws focused visible; do
    [ "$focused" = true ] && FOCUSED=$ws
    [ "$visible" = true ] && VISIBLE+="$ws "
done <<< "$STATES"

# APPS[i] = space-separated app icon ligatures for workspace i
APPS=()
while IFS='|' read -r ws app; do
    case $ws in [1-9]) ;; *) continue ;; esac
    # Cargo-built Alacritty reports a lowercase name; icon_map only knows "Alacritty"
    [ "$app" = alacritty ] && app=Alacritty
    __icon_map "$app"
    APPS[ws]+=" $icon_result"
done <<< "$PAIRS"

# Build a single batched sketchybar command for all 9 workspaces
args=()

for i in {1..9}; do
    ICONS=${APPS[i]# }
    BG_DRAWING=on

    if [ "$i" = "$FOCUSED" ]; then
        COLOR=$LABEL_ACTIVE BG=$BG_ACTIVE
    elif [[ $VISIBLE == *" $i "* ]]; then
        # Shown on another monitor
        COLOR=$LABEL_OTHER_DISPLAY BG=$BG_OTHER_DISPLAY
    elif [ -n "$ICONS" ]; then
        COLOR=$LABEL_INACTIVE BG=$BG_INACTIVE
    else
        COLOR=$LABEL_EMPTY BG=$BG_INACTIVE BG_DRAWING=off
    fi

    LABEL_DRAWING=off
    [ -n "$ICONS" ] && LABEL_DRAWING=on

    args+=(--set workspace."$i"
        icon.color="$COLOR"
        label="$ICONS"
        label.color="$COLOR"
        label.drawing=$LABEL_DRAWING
        background.color="$BG"
        background.drawing=$BG_DRAWING
        drawing=on)
done

# Single IPC call to sketchybar instead of 9 separate ones
sketchybar "${args[@]}"
