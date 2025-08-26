#!/bin/bash

BASE_BRANCH=$1
HEAD_BRANCH=$2

echo "Checking for Sierra updates in $HEAD_BRANCH against $BASE_BRANCH."

# Assuming all updates are provided as inputs - finding if they are in any of the relevant crates.
MERGE_BASE=$(git merge-base "$BASE_BRANCH" "$HEAD_BRANCH")

echo "Found merge base: $MERGE_BASE"

echo "Found the following file with changes from relevant crates in $MERGE_BASE..$HEAD_BRANCH:"
git diff --name-only "$MERGE_BASE".."$HEAD_BRANCH" | grep -q -E \
    -e 'crates/cairo-lang-sierra/' \
    -e 'crates/cairo-lang-sierra-gas/' \
    -e 'crates/cairo-lang-sierra-ap-change/' \
    -e 'crates/cairo-lang-sierra-to-casm/'
if [ $? -eq 0 ]; then
    # If so, check if the commit message contains an explanation tag.
    git log "$MERGE_BASE".."$HEAD_BRANCH" --pretty=format:"%b" | grep \
        -e 'SIERRA_UPDATE_NO_CHANGE_TAG=' \
        -e 'SIERRA_UPDATE_PATCH_CHANGE_TAG=' \
        -e 'SIERRA_UPDATE_MINOR_CHANGE_TAG=' \
        -e 'SIERRA_UPDATE_MAJOR_CHANGE_TAG=' >/dev/null
    if [ $? -eq 0 ]; then
        exit 0
    fi
    echo "We have a change in one of the Sierra crates - an explanation tag is required."
    echo "If there's no actual change:"
    echo " - Add 'SIERRA_UPDATE_NO_CHANGE_TAG=<reason>'."
    echo "If there is a change - but it does not affect which code will be compiled:"
    echo " - Add 'SIERRA_UPDATE_PATCH_CHANGE_TAG=<reason>'."
    echo "If there is a change, but previously compilable code is still compilable:"
    echo " - Add 'SIERRA_UPDATE_MINOR_CHANGE_TAG=<reason>'."
    echo "If there is a breaking change, and old code is no longer compilable:"
    echo " - Add 'SIERRA_UPDATE_MAJOR_CHANGE_TAG=<reason>'."
    exit 1
else
    # If not, we don't care.
    exit 0
fi
