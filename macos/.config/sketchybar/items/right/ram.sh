ram=(
    "${right_items_common[@]}"
    icon="$SYSTEM_RAM"
    icon.color="$RAM_ICON_COLOR"
    update_freq=5
    script="$PLUGIN_DIR/right/ram.sh"
    display=1
)

# Clicks go through the script (mouse.clicked) so it can tell left from right
sketchybar --add item ram right \
    --set ram "${ram[@]}" \
    --subscribe ram mouse.clicked
