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
    cargo yank --version "$VERSION" "$CRATE" || echo "Warning: failed to yank $CRATE@$VERSION (may not have been published)."
done
