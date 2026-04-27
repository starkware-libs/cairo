#!/bin/bash
# Yanks all crates from crates.io for a given version.
# Usage: yank_crates.sh <version>
#
# Must be run after a failed release_crates.sh to undo partially published crates.
# Crates that were not published are skipped with a warning.

if [ -z "$1" ]; then
    echo "Error: version argument is required (e.g. 2.17.0)."
    exit 1
fi
VERSION="$1"

source "$(dirname "$0")/crates_list.sh"

for CRATE in "${CRATES[@]}"; do
    output=$(cargo yank --version "$VERSION" "$CRATE" 2>&1)
    if [ $? -ne 0 ]; then
        if echo "$output" | grep -qi "not found\|does not exist"; then
            echo "Skipping $CRATE@$VERSION (not published)."
        else
            echo "$output" >&2
            exit 1
        fi
    fi
done
