#!/usr/bin/env bash

MODE=$(aerospace list-modes --current 2>/dev/null)

if [ -z "$MODE" ] || [ "$MODE" = main ]; then
    sketchybar --set "$NAME" drawing=off
else
    sketchybar --set "$NAME" drawing=on label="$(printf '%s' "$MODE" | tr '[:lower:]' '[:upper:]')"
fi
