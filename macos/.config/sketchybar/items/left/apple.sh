POPUP_OFF="sketchybar --set apple.logo popup.drawing=off"
POPUP_CLICK_SCRIPT="sketchybar --set \$NAME popup.drawing=toggle"

popup_props=(
    popup.height=30
)
apple_logo=(
    "${left_items_common[@]}"
    "${popup_props[@]}"
    icon="$APPLE"
    icon.color="$APPLE_ICON_COLOR"
    icon.font.size=18
    label.drawing=off
    icon.y_offset=2
    click_script="$POPUP_CLICK_SCRIPT"
)
pupup_item=(
    # icon.padding_left=6
    icon.padding_right=6
    # label.padding_left=0
    # label.padding_right=6
    background.padding_right=10
    background.padding_left=10
    background.drawing=off
)

sketchybar --add item apple.logo left \
    --set apple.logo "${apple_logo[@]}" \
    \
    --add item apple.prefs popup.apple.logo \
    --set apple.prefs "${pupup_item[@]}" \
    icon="$PREFERENCES" \
    label="Preferences" \
    icon.color="$APPLE_POPUP_COLOR" \
    label.color="$APPLE_POPUP_COLOR" \
    click_script="open -a 'System Settings' ; $POPUP_OFF" \
    \
    --add item apple.activity popup.apple.logo \
    --set apple.activity "${pupup_item[@]}" \
    icon="$ACTIVITY" \
    label="Activity" \
    icon.color="$APPLE_POPUP_COLOR" \
    label.color="$APPLE_POPUP_COLOR" \
    click_script="open -a 'Activity Monitor' ; $POPUP_OFF" \
    \
    --add item apple.lock popup.apple.logo \
    --set apple.lock "${pupup_item[@]}" \
    icon="$LOCK" \
    label="Lock Screen" \
    icon.color="$APPLE_POPUP_COLOR" \
    label.color="$APPLE_POPUP_COLOR" \
    click_script="$POPUP_OFF ; $SCRIPTS_DIR/lock_screen.sh"