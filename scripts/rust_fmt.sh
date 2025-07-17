#!/bin/bash

export RUSTUP_TOOLCHAIN="${RUSTUP_TOOLCHAIN:-nightly-2025-07-14}"

cargo fmt --all -- "$@"
