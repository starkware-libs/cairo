#!/bin/bash

cargo run --bin cairo-test --release -- \
    crates/cairo-lang-starknet/cairo_level_tests/ --starknet
