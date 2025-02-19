#!/usr/bin/env bash

set -euo pipefail

SCARB_REPO="https://github.com/software-mansion/scarb"

CURRENT_VERSION='2.10.1'
NEW_VERSION="$@"

# NOTE: These two functions were copied from asdf-scarb.
# https://github.com/software-mansion/asdf-scarb/blob/9d5937bbfee50c6df697fd45a0ae9cc98f6b88a0/lib/utils.bash#L22-L31
sort_versions() {
    sed 'h; s/[+-]/./g; s/.p\([[:digit:]]\)/.z\1/; s/$/.z/; G; s/\n/ /' |
        LC_ALL=C sort -t. -k 1,1 -k 2,2n -k 3,3n -k 4,4n -k 5,5n | awk '{print $2}'
}

list_github_tags() {
    git ls-remote --tags --refs "$1" |
        grep -o 'refs/tags/v.*' | cut -d/ -f3- |
        sed 's/^v//'
}

check_scarb_version_sync() {
    local all_scarb_versions
    all_scarb_versions="$(list_github_tags "$SCARB_REPO" | sort_versions)"

    local latest_scarb_version
    latest_scarb_version="$(echo "$all_scarb_versions" | tail -n1)"

    local wrapped_all_scarb_versions
    wrapped_all_scarb_versions="$(echo $all_scarb_versions | awk '{print "`" $1 "`"}')"
    if [[ "$wrapped_all_scarb_versions" == *"\`$NEW_VERSION\`"* ]]; then
        echo "error: cairo $NEW_VERSION = scarb $NEW_VERSION"
        echo "help: the latest Scarb release is: $latest_scarb_version"
        exit 1
    fi

    local expected_order
    expected_order="$(printf "%s\n%s" "$latest_scarb_version" "$NEW_VERSION")"
    local actual_order
    actual_order="$(echo "$expected_order" | sort_versions)"

    if [ "$expected_order" != "$actual_order" ]; then
        echo "error: cairo $NEW_VERSION < scarb $latest_scarb_version"
        exit 1
    fi
}

check_scarb_version_sync

sed -i "s/$CURRENT_VERSION/$NEW_VERSION/g" \
    $(find . -type f -iname "*.toml") \
    ./scripts/bump_version.sh
