#!/bin/bash

export RUSTUP_TOOLCHAIN="${RUSTUP_TOOLCHAIN:-nightly-2026-11-02}"

cargo fmt --all -- "$@"
