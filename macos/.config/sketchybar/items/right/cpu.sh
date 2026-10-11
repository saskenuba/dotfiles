cpu=(
    "${right_items_common[@]}"
    icon="$SYSTEM_CPU"
    icon.color="$CPU_ICON_COLOR"
    icon.font.size=21
    update_freq=5
    script="$PLUGIN_DIR/right/cpu.sh"
    display=1
)

# Clicks go through the script (mouse.clicked) so it can tell left from right
sketchybar --add item cpu right \
    --set cpu "${cpu[@]}" \
    --subscribe cpu mouse.clicked
