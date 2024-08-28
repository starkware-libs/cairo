#!/bin/bash

export RUSTUP_TOOLCHAIN="${RUSTUP_TOOLCHAIN:-nightly-2024-08-22}"

cargo fmt --all -- "$@"
