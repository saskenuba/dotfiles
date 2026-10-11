#!/usr/bin/env bash

############################################
# Theme — picked by the `theme` script, which stores the name outside the
# repo. themes/<name>.sh defines the palette; this file maps it onto items.
# `read` is a builtin, so plugins sourcing this on every event don't fork.

read -r THEME 2>/dev/null < "${XDG_STATE_HOME:-$HOME/.local/state}/theme/current"
[ -f "$CONFIG_DIR/themes/$THEME.sh" ] || THEME=gruvbox-material-dark
source "$CONFIG_DIR/themes/$THEME.sh"

############################################
# Default Colors

DEFAULT_LABEL_COLOR="$FG_DIM"
DEFAULT_ICON_COLOR="$FG_SOFT"
DEFAULT_BACKGROUND_COLOR=0x00000000
DEFAULT_BAR_COLOR="$BAR_BG"

############################################
# Items Colors — muted, fewer hues

APPLE_ICON_COLOR="$RED"
APPLE_POPUP_COLOR="$FG"

WIFI_ICON_COLOR="$BLUE"
WIFI_DISCONNECTED_ICON_COLOR="$FG_DIM"

VOLUME_ICON_COLOR="$FG"

RAM_ICON_COLOR="$GREEN"
CPU_ICON_COLOR="$BLUE"
BATTERY_ICON_COLOR="$GREEN"

CLOCK_ICON_COLOR="$FG"
CLOCK_LABEL_COLOR="$FG_DIM"

FRONT_APP_LABEL_COLOR="$FG"

# AeroSpace binding mode indicator (only shown outside 'main')
MODE_LABEL_COLOR="$ATTENTION_FG"
MODE_BG_COLOR="$ATTENTION_BG"

############################################
# Workspace Colors — keep vivid for active (functional signal)

WS_DEFAULT_COLOR="$FG_FAINT"
WS_ACTIVE_ICON_COLOR="$SELECTION_FG"
WS_ACTIVE_LABEL_COLOR="$SELECTION_FG"
WS_ACTIVE_BG_COLOR="$SELECTION_BG"
WS_ACTIVE_ANOTHER_DISPLAY_ICON_COLOR="$NOTICE_FG"
WS_ACTIVE_ANOTHER_DISPLAY_LABEL_COLOR="$NOTICE_FG"
WS_ACTIVE_ANOTHER_DISPLAY_BG_COLOR="$NOTICE_BG"
WS_WIHT_CLIENTS_ICON_COLOR="$FG_SOFT"
WS_WIHT_CLIENTS_LABEL_COLOR="$FG_SOFT"
WS_WIHT_CLIENTS_BG_COLOR="$SURFACE"
WS_EMPTY_LABEL_COLOR="$FG_FAINTER"

############################################
# Popup Colors

POPUP_BORDER_COLOR="$POPUP_BORDER"
POPUP_BACKGROUND_COLOR="$POPUP_BG"
