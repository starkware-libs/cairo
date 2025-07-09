#!/bin/bash

export RUSTUP_TOOLCHAIN="${RUSTUP_TOOLCHAIN:-nightly-2025-07-08}"

cargo clippy "$@" --all-targets --all-features -- -D warnings -D future-incompatible \
    -D nonstandard-style -D rust-2018-idioms -D unused
