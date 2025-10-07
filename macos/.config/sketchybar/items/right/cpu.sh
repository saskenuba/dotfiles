cpu=(
    "${right_items_common[@]}"
    icon="$SYSTEM_CPU"
    icon.color="$CPU_ICON_COLOR"
    icon.font.size=21
    update_freq=5
    script="$PLUGIN_DIR/right/cpu.sh"
    click_script="$SCRIPTS_DIR/run_external_bash.sh '/opt/homebrew/bin/btop && exit'"
    display=1
)

sketchybar --add item cpu right \
    --set cpu "${cpu[@]}"
