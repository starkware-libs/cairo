#!/bin/bash

cargo +nightly-2022-11-03 run --bin cairo-test -- \
    --path crates/cairo-lang-starknet/cairo_level_tests/ --starknet
