#!/bin/bash

cargo run --profile=ci-dev --bin cairo-test -- \
    crates/cairo-lang-starknet/cairo_level_tests/ --starknet
