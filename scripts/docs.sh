#!/bin/bash

export RUSTUP_TOOLCHAIN="${RUSTUP_TOOLCHAIN:-nightly-2024-08-22}"

RUSTDOCFLAGS="-Dwarnings" cargo doc --document-private-items --no-deps --all-features
