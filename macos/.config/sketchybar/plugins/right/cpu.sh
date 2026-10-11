#!/usr/bin/env bash

if [ "$SENDER" = "mouse.clicked" ]; then
    "$CONFIG_DIR/scripts/system_monitor.sh" "$BUTTON"
    exit 0
fi

# Uses sysctl instead of top -l 1 (near-instant vs ~1s)
CPU_LOAD=$(sysctl -n vm.loadavg | awk '{print $2}')
NUM_CORES=$(sysctl -n hw.ncpu)
CPU_PERCENT=$(echo "$CPU_LOAD $NUM_CORES" | awk '{printf "%02d", ($1 / $2) * 100}')

sketchybar --set "$NAME" label="${CPU_PERCENT}%"