#!/bin/bash

cargo run --bin cairo-test -- \
    crates/cairo-lang-starknet/cairo_level_tests/ --starknet && \
cargo run --bin cairo-test -- \
    crates/cairo-lang-test-runner/test_data/ --starknet
