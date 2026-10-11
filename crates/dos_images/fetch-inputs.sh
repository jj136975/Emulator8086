#!/usr/bin/env bash
# Fetches what the MS-DOS 4.00 build takes in (crates/dos_images) into a
# directory, and fails unless each input is the one pins.env, beside this
# script, pins:
#
#   <dir>/freedos/x86BOOT.img   FreeDOS 1.4's boot floppy, the build host
#   <dir>/msdos/v4.0/           MS-DOS 4.00's source, as restored
#   <dir>/doom8088/             D8M13H.EXE, DOOM1.WAD and README.TXT of
#                               Doom8088's release, and COPYING, its GPL text
#
# Usage: bash crates/dos_images/fetch-inputs.sh <dir>
#
# Then the tool builds the disks from them, into <out>:
#
#   cargo run --release -p dos_images -- --freedos <dir>/freedos \
#       --msdos <dir>/msdos/v4.0 --doom8088 <dir>/doom8088 --out <out>
#
# What comes from the network is data: every download goes into a
# directory of its own below <dir>/downloads, and nothing of it is run.
# <dir> has to be new or empty, so that nothing of another fetch is taken
# for this one's. It runs where bash, curl, sha256sum, unzip and git do:
# GitHub's Ubuntu runner, a Linux machine, Git Bash on Windows.
set -euo pipefail

if [ $# -ne 1 ]; then
    echo "usage: $0 <dir>" >&2
    exit 2
fi
here=$(cd "$(dirname "$0")" && pwd)
# shellcheck source=pins.env
. "$here/pins.env"

dir=$1
if [ -e "$dir" ] && [ -n "$(ls -A "$dir")" ]; then
    echo "$dir is not empty: the inputs go into a new or empty directory" >&2
    exit 2
fi
mkdir -p "$dir"
dir=$(cd "$dir" && pwd)

# check <sha256> <file>: fails unless the file has that SHA-256.
check() {
    echo "$1  $2" | sha256sum --check --strict
}

# download <url> <file>: the file at <url>, saved as <file> in a directory
# that is made for it.
download() {
    mkdir "$(dirname "$2")"
    curl --fail --silent --show-error --location --retry 3 --output "$2" "$1"
}

# FreeDOS: the boot floppy alone, out of the Floppy Edition's zip.
mkdir "$dir/downloads"
zip="$dir/downloads/freedos/${FREEDOS_ZIP_URL##*/}"
download "$FREEDOS_ZIP_URL" "$zip"
check "$FREEDOS_ZIP_SHA256" "$zip"
mkdir "$dir/freedos"
unzip -q -j "$zip" "$FREEDOS_BOOT" -d "$dir/freedos"
check "$FREEDOS_BOOT_SHA256" "$dir/freedos/${FREEDOS_BOOT##*/}"

# Doom8088: the Mode 13h build, the WAD and the README out of the release's
# zip, and the GPL text of the repository at the tag's commit as COPYING.
zip="$dir/downloads/doom8088/${DOOM8088_ZIP_URL##*/}"
download "$DOOM8088_ZIP_URL" "$zip"
check "$DOOM8088_ZIP_SHA256" "$zip"
mkdir "$dir/doom8088"
unzip -q -j "$zip" D8M13H.EXE DOOM1.WAD README.TXT -d "$dir/doom8088"
check "$DOOM8088_EXE_SHA256" "$dir/doom8088/D8M13H.EXE"
check "$DOOM8088_WAD_SHA256" "$dir/doom8088/DOOM1.WAD"
check "$DOOM8088_README_SHA256" "$dir/doom8088/README.TXT"
license="$dir/downloads/doom8088-license/LICENSE"
download "$DOOM8088_LICENSE_URL" "$license"
check "$DOOM8088_LICENSE_SHA256" "$license"
cp "$license" "$dir/doom8088/COPYING"

# MS-DOS 4.00: v4.0 of the pinned commit, without the history and without
# the rest of the repository (a few megabytes). The files are checked out
# as committed: on Windows, git would give the ones committed with LF line
# endings CRLF ones, and the build would take in other sources.
msdos="$dir/msdos"
git init --quiet "$msdos"
git -C "$msdos" config core.autocrlf false
echo '* -text' > "$msdos/.git/info/attributes"
git -C "$msdos" remote add origin "$MSDOS_REPOSITORY"
git -C "$msdos" sparse-checkout set v4.0
git -C "$msdos" fetch --quiet --depth 1 --filter=blob:none origin "$MSDOS_COMMIT"
git -C "$msdos" checkout --quiet FETCH_HEAD
commit=$(git -C "$msdos" rev-parse HEAD)
tree=$(git -C "$msdos" rev-parse HEAD:v4.0)
if [ "$commit" != "$MSDOS_COMMIT" ] || [ "$tree" != "$MSDOS_TREE" ]; then
    echo "MS-DOS source: commit $commit with v4.0 $tree, not $MSDOS_COMMIT with $MSDOS_TREE" >&2
    exit 1
fi
if [ -n "$(git -C "$msdos" status --porcelain)" ]; then
    echo "MS-DOS source: the files checked out are not those of $MSDOS_COMMIT" >&2
    exit 1
fi
echo "MS-DOS source: v4.0 of $MSDOS_COMMIT, tree $MSDOS_TREE"

echo "The inputs are in $dir."
