#!/usr/bin/env bash

# This script refreshes all workspace indicators
# It will automatically show workspaces that have windows

for i in {1..9}; do
    "$CONFIG_DIR/plugins/left/aerospace_workspaces.sh" "$i"
done
