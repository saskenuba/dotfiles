#!/usr/bin/zsh -f
set -eo pipefail
IFS=$'\n\t'

# ---------------------------------------------------------------------------------------------------
# Generates docset for the desired rust crate, and installs it to work with Zeal (offline doc tool).
# Optionally accepts an image link to add an icon.
#
# Author: Martin Mariano
#
# usage: gendocs <git-crate-repo-url> ( --no-deps -p <package> | --all ) [ -i | --image image-url ]
#
# Example: ./gendocs.sh https://github.com/SergioBenitez/Rocket -p rocket -p rocket_contrib --no-deps --image https://rocket.rs/v0.4/images/logo-boxed.png
#
# In order to work correctly, cargo-docset crate and imagemagick package need to be installed.
# Zeal docset directory can be modified at ZEAL_DIR variable.
#
# This was created to work on zsh shell, but should be easily modified to work on bash/sh.
#
# ---------------------------------------------------------------------------------------------------

DOCSET_ARGS=()
ICON_SIZE=16x16
IMAGE_PATH="/dev/shm/icon.png"
TMP_DIR="/dev/shm"
ZEAL_DIR="${HOME}/.local/share/Zeal/Zeal/docsets/"

log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1"
}

logo_to_icon() {
    wget -q "$1" -O - | convert png:- -resize $ICON_SIZE $IMAGE_PATH
}

if [[ "$#" -lt 1 ]]; then
    echo 'usage: gendocs <git-crate-repo-url> ( --no-deps -p <package> | --all ) [ -i | --image image-url ]'
    exit 1
fi

GIT_CLONE_URL=$1
IMAGE_LINK=0

while [[ "$#" -gt 0 ]]; do
    case $2 in
    "-i" | "--image")
        shift
        IMAGE_LINK="$2"
        ;;
    "") ;;
    *) ; DOCSET_ARGS+=("$2") ;;
    esac
    shift
done

echo
log "Arguments passed to cargo docset: ${DOCSET_ARGS[*]}"
echo

package_name=$(basename "$GIT_CLONE_URL" .git)
output_dir="$TMP_DIR/$package_name"

if ! git clone "$GIT_CLONE_URL" "$output_dir" 2>/dev/null; then
    log "Module is already cloned. Skipping"
    echo
    exit 1
fi

cd "$output_dir"
cargo docset "${DOCSET_ARGS[@]}"

docset_basename=0
for file in "${output_dir}/target/docset/"*; do
    echo
    echo "$file"
    echo

    docset_basename=$(basename "$file")
    cp -r "$file" "$ZEAL_DIR"
done

echo
log "Generated docset file: ${docset_basename}"
echo

logo_to_icon "$IMAGE_LINK" && mv $IMAGE_PATH "${ZEAL_DIR}/${docset_basename}/."
log "Finished. Restart Zeal to check the changes."

# cleanup
rm -rf "$output_dir"
