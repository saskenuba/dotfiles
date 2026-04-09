#!/usr/bin/env bash

if [[ "$SENDER" == "mouse."* ]]; then
    case "$SENDER" in
        "mouse.clicked")
            # echo "sender: $SENDER, button: $BUTTON, modifier: $MODIFIER, scroll_delta: $SCROLL_DELTA" > /tmp/logs.txt
            case "$BUTTON" in
                "left")
                    "$CONFIG_DIR"/scripts/run_external_bash.sh '/opt/homebrew/bin/btop && exit'
                    ;;
                "right")
                    open -a "Activity Monitor"
                    ;;
            esac
            ;;
    esac
fi

# Uses sysctl instead of top -l 1 (near-instant vs ~1s)
CPU_LOAD=$(sysctl -n vm.loadavg | awk '{print $2}')
NUM_CORES=$(sysctl -n hw.ncpu)
CPU_PERCENT=$(echo "$CPU_LOAD $NUM_CORES" | awk '{printf "%02d", ($1 / $2) * 100}')

sketchybar --set "$NAME" label="${CPU_PERCENT}%"