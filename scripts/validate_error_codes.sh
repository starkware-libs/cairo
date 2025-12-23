#!/bin/bash

# Validate that each error_code! appears at most once in diagnostic.rs files

# Find all error_code! usages in diagnostic.rs files
usages=$(find crates -name diagnostic.rs -exec grep "error_code!(E[0-9]\+)" {} \; | sed 's/.*error_code!(\(E[0-9]\+\)).*/\1/')

duplicates=$(echo "$usages" | sort | uniq -d)

if [ -n "$duplicates" ]; then
    echo "Duplicate error codes found in diagnostic.rs files:"
    echo "$duplicates"
    exit 1
else
    echo "All error codes in diagnostic.rs files are unique."
fi
