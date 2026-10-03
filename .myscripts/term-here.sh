#!/bin/bash
# Open a new Alacritty window in the folder the focused window is "in".
#
#   focused Alacritty -> cwd of that window's login shell
#   focused Finder    -> folder shown by the front window
#   anything else     -> $HOME
#
# Bound to ctrl-shift-enter in ~/.aerospace.toml

set -uo pipefail

# AeroSpace's exec-and-forget does not run a login shell, so don't rely on the
# interactive PATH: aerospace lives in /opt/homebrew/bin and lsof in /usr/sbin.
PATH="/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH"

ALACRITTY="$HOME/.cargo/bin/alacritty"

# cwd of a pid; empty when the process is gone or unreadable (root-owned, e.g. /usr/bin/login)
cwd_of() {
    lsof -a -d cwd -p "$1" -Fn 2>/dev/null | sed -n 's/^n//p' | head -1
}

# Descendant pids of $1 in breadth-first order.
# pgrep -P is unreliable here (returns nothing even for same-user pairs),
# so walk a single ps snapshot instead.
descendants() {
    ps -eo pid=,ppid= | awk -v root="$1" '
        { pid[NR] = $1; parent[$1] = $2; n = NR }
        END {
            queue[1] = root; last = 1
            for (i = 1; i <= last; i++)
                for (j = 1; j <= n; j++)
                    if (parent[pid[j]] == queue[i]) { queue[++last] = pid[j]; print pid[j] }
        }'
}

# Alacritty spawns alacritty -> login -> zsh, so the first shell in BFS order is
# the window's login shell. That is the one tracking the user's `cd`; deeper
# shells are usually transient children of whatever is running (claude, git, ...)
# and their cwd is not what the user means by "here".
login_shell_cwd() {
    local pid name
    for pid in $(descendants "$1"); do
        name=$(ps -o comm= -p "$pid" 2>/dev/null)
        case ${name##*/} in
            -zsh | zsh | -bash | bash | -fish | fish | -sh | sh)
                cwd_of "$pid"
                return
                ;;
        esac
    done
}

finder_cwd() {
    osascript -e '
        tell application "Finder"
            if (count of windows) is 0 then return ""
            return POSIX path of (target of front window as alias)
        end tell' 2>/dev/null
}

focused=$(aerospace list-windows --focused --format '%{app-pid}|%{app-name}' 2>/dev/null)
app_pid=${focused%%|*}
app_name=${focused#*|}

case $app_name in
    [aA]lacritty) target=$(login_shell_cwd "$app_pid") ;;
    Finder) target=$(finder_cwd) ;;
    *) target="" ;;
esac

[ -d "${target:-}" ] || target=$HOME

exec "$ALACRITTY" --working-directory "$target"
