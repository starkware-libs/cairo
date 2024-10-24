#!/bin/bash

export RUSTUP_TOOLCHAIN="${RUSTUP_TOOLCHAIN:-nightly-2024-09-21}"

cargo fmt --all -- "$@"
