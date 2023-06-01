#!/bin/bash

# Assuming all updates are provided as inputs - finding if they are in any of the relevant crates.
echo $@ | grep -e 'crate/cairo-lang-sierra/' -e 'crate/cairo-lang-sierra-gas/' -e 'crate/cairo-lang-sierra-ap-change/' -e 'crate/cairo-lang-sierra-to-casm/' > /dev/null
if [ $? -eq 0 ]; then
    # If so, check if the commit message contains an explaination tag.
    git log -1 --pretty=%B | grep SIERRA_UPDATED_REASON_TAG= > /dev/null
else
    # If not, we don't care.
    exit 0
fi
