#!/bin/bash

export RUSTUP_TOOLCHAIN="${RUSTUP_TOOLCHAIN:-nightly-2025-06-20}"

RUSTDOCFLAGS="-Dwarnings" cargo doc --document-private-items --no-deps --all-features
