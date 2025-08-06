#!/bin/bash

export RUSTUP_TOOLCHAIN="${RUSTUP_TOOLCHAIN:-nightly-2025-07-14}"

RUSTDOCFLAGS="-Dwarnings" cargo doc --document-private-items --no-deps --all-features
