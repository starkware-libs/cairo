#!/bin/bash

export RUSTUP_TOOLCHAIN="${RUSTUP_TOOLCHAIN:-nightly-2025-02-16}"

RUSTDOCFLAGS="-Dwarnings" cargo doc --document-private-items --no-deps --all-features
