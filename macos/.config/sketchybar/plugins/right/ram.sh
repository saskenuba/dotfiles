#!/usr/bin/env bash

# Parse memory_pressure output more efficiently
memory_output=$(memory_pressure)

# Get total memory in GB
total_gb=$(sysctl -n hw.memsize | awk '{printf "%.1f", $1/1073741824}')

# Get free percentage and calculate used
free_percent=$(echo "$memory_output" | grep "System-wide memory free percentage:" | awk '{print $5}' | sed 's/%//')
used_percent=$((100 - free_percent))

# Calculate used GB from percentage
used_gb=$(echo "scale=1; $total_gb * $used_percent / 100" | bc)

LABEL="${used_gb}G / ${total_gb}G"
sketchybar --set "$NAME" label="$LABEL"