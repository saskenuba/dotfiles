#!/bin/bash

source "$CONFIG_DIR/icons.sh"

# WiFi Icons
readonly WIFI_CONNECTED_ICON="$WIFI_CONNECTED"
readonly WIFI_DISCONNECTED_ICON="$WIFI_DISCONNECTED"
readonly WIFI_WEAK_ICON="$WIFI_CONNECTED"
readonly WIFI_MEDIUM_ICON="$WIFI_CONNECTED"
readonly WIFI_STRONG_ICON="$WIFI_CONNECTED"

# Constants
readonly NOT_CONNECTED="Not Connected"
readonly DISCONNECTED_LABEL="Disconnected"

# Exit codes
readonly SUCCESS=0
readonly ERROR=1

# Get current WiFi network name
get_wifi_name() {
    if system_profiler SPAirPortDataType 2>/dev/null | grep -q "Status: Connected"; then
        system_profiler SPAirPortDataType 2>/dev/null | awk '
            /Current Network Information:/ { 
                found = 1
                next 
            }
            found && /^[[:space:]]+[^:]+:$/ {
                gsub(/^[[:space:]]+|:$/, "")
                print
                exit
            }
        '
    else
        echo "$NOT_CONNECTED"
    fi
}

# Get WiFi signal strength in dBm
get_wifi_signal() {
    system_profiler SPAirPortDataType 2>/dev/null | \
        grep "Signal / Noise" | \
        awk '{print $4}' | \
        sed 's/dBm//' | \
        tr -d '-'
}

# Determine icon based on signal strength
get_signal_icon() {
    local signal="$1"
    
    if [[ -z "$signal" ]]; then
        echo "$WIFI_CONNECTED_ICON"
        return
    fi
    
    if (( signal > 70 )); then
        echo "$WIFI_WEAK_ICON"
    elif (( signal > 50 )); then
        echo "$WIFI_MEDIUM_ICON"
    else
        echo "$WIFI_STRONG_ICON"
    fi
}

local wifi_name
local signal
local icon
local label
    
wifi_name=$(get_wifi_name)
    
if [[ "$wifi_name" == "$NOT_CONNECTED" ]]; then
    icon="$WIFI_DISCONNECTED_ICON"
    label="$DISCONNECTED_LABEL"
else
    signal=$(get_wifi_signal)
    icon=$(get_signal_icon "$signal")
    label="$wifi_name"
fi
    
# Update SketchyBar with minimal properties
sketchybar --set wifi \
    icon="$icon" \
    label="$label"

