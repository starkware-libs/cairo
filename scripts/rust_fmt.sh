#!/bin/bash

export RUSTUP_TOOLCHAIN="${RUSTUP_TOOLCHAIN:-nightly-2025-11-17}"

cargo fmt --all -- "$@"
