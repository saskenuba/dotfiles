#!/usr/bin/env bash
# Lock the session immediately, same as ctrl-cmd-q.
#
# `pmset displaysleepnow` only blanks the display: the session stays unlocked
# until the "require password after..." delay runs out (5 minutes here).
# SACLockScreenImmediate is the private login.framework call behind ctrl-cmd-q;
# calling it needs no Accessibility permission, unlike faking the keystroke.

exec /usr/bin/python3 -c "import ctypes; ctypes.CDLL('/System/Library/PrivateFrameworks/login.framework/Versions/Current/login').SACLockScreenImmediate()"
