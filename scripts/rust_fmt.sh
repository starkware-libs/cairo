#!/bin/bash

export RUSTUP_TOOLCHAIN="${RUSTUP_TOOLCHAIN:-nightly-2025-04-04}"

cargo fmt --all -- "$@"
