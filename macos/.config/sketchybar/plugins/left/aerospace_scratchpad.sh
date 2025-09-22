#!/usr/bin/env bash

source "$CONFIG_DIR/colors.sh"
source "$CONFIG_DIR/settings.sh"

function load_scratchpad_info() {
    SCRATCHPAD_APPS=$(aerospace list-windows --workspace "NSP" 2>/dev/null)
    SCRATCHPAD_COUNT=$(echo "$SCRATCHPAD_APPS" | grep -v '^$' | wc -l | awk '{print $1}')
}

function refresh() {
    load_scratchpad_info

    if [ -z "$SCRATCHPAD_APPS" ]; then
        return
    fi

    args=(--remove '/scratchpad.popup\.*/' )

    local counter=0
    while IFS='|' read -r id label rest; do
        # trim leading/trailing spaces
        label="${label#"${label%%[![:space:]]*}"}"
        label="${label%"${label##*[![:space:]]}"}"

        # Get icon for the application
        icon=$(get_app_icon "$label")

        args+=(
            --add item "$NAME".popup."$counter" popup."$NAME"
            --set "$NAME".popup."$counter" \
            label="$label" \
            icon="$icon" \
            icon.padding_right=6 \
            label.color=0xff82aaff \
            background.padding_right=10 \
            background.padding_left=10 \
            background.drawing=off
        )
        counter=$((counter + 1))

    done <<< "$SCRATCHPAD_APPS"

    sketchybar -m "${args[@]}" >/dev/null
}

function get_app_icon() {
    case "$1" in
        "kitty") echo "🐱" ;;
        "Terminal") echo "💻" ;;
        "Google Chrome" | "Brave Browser") echo "🌐" ;;
        "Discord") echo "💬" ;;
        "Slack") echo "💼" ;;
        "Code" | "Visual Studio Code") echo "📝" ;;
        *) echo "📱" ;;
    esac
}

function popup() {
    sketchybar --set "$NAME" popup.drawing="$1"
}

case "$SENDER" in
    "mouse.entered")
        popup on
        exit 0
        ;;
    "mouse.exited" | "mouse.exited.global" | "mouse.clicked")
        popup off
        exit 0
        ;;
    *)
        refresh
        ;;
esac

if [ -z "$SCRATCHPAD_COUNT" ]; then
    load_scratchpad_info
fi

sketchybar --set "$NAME" label="$SCRATCHPAD_COUNT"
