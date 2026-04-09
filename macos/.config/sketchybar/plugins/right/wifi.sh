#!/bin/bash

source "$CONFIG_DIR/icons.sh"

# Uses ipconfig instead of system_profiler (~50x faster)
# system_profiler SPAirPortDataType takes 2-5s; ipconfig is near-instant

INTERFACE="en0"
SUMMARY=$(ipconfig getsummary "$INTERFACE" 2>/dev/null)

if [[ -z "$SUMMARY" ]]; then
    sketchybar --set wifi icon="$WIFI_DISCONNECTED" label="Disconnected"
    exit 0
fi

SSID=$(echo "$SUMMARY" | awk -F': ' '/^[[:space:]]+SSID :/{print $2}')

if [[ -z "$SSID" || "$SSID" == "NONE" ]]; then
    sketchybar --set wifi icon="$WIFI_DISCONNECTED" label="Disconnected"
    exit 0
fi

sketchybar --set wifi icon="$WIFI_CONNECTED" label="$SSID"

