#!/usr/bin/env bash

# Match Activity Monitor's "Memory Used":
# app memory (anonymous - purgeable) + wired + compressed
total_bytes=$(sysctl -n hw.memsize)

LABEL=$(vm_stat | awk -v total="$total_bytes" '
    /page size of/                 { page = $8 }
    /^Anonymous pages/             { anon = $3 }
    /^Pages purgeable/             { purgeable = $3 }
    /^Pages wired down/            { wired = $4 }
    /^Pages occupied by compressor/ { comp = $5 }
    END {
        used = (anon - purgeable + wired + comp) * page
        printf "%.1fG / %.1fG", used / 1073741824, total / 1073741824
    }')

sketchybar --set "$NAME" label="$LABEL"
