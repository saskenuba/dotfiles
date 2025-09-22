#!/usr/bin/env bash

# Create workspace items for 1-9 (no individual scripts)
for i in {1..9}; do
    sketchybar --add item workspace.$i left \
        --set workspace.$i \
        label="$i:0" \
        label.font="$LABEL_FONT" \
        label.color="$ORANGE" \
	background.corner_radius=4 \
	label.padding_left=4 \
	label.padding_right=4 \
	padding_right=4 \
        icon.drawing=off \
        click_script="aerospace workspace $i" \
        drawing=off
done

# Single monitor that updates everything
sketchybar --add item workspace_monitor left \
    --set workspace_monitor \
    drawing=off \
    update_freq=2 \
    script="$PLUGIN_DIR/left/aerospace_workspaces.sh" \
    --subscribe workspace_monitor workspace_change
