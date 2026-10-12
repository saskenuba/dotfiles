#!/usr/bin/env bash

if [ "$SENDER" = "mouse.clicked" ]; then
    "$CONFIG_DIR/scripts/system_monitor.sh" "$BUTTON"
    exit 0
fi

# Real utilization (user + sys). top's first sample is the since-boot average,
# so take the second one; the ~1s it costs is fine since scripts run async.
CPU_PERCENT=$(top -l 2 -n 0 -s 1 | awk '/CPU usage/ {u=$3; s=$5} END {printf "%02d", u+s}')

sketchybar --set "$NAME" label="${CPU_PERCENT}%"