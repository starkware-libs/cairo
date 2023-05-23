#!/bin/bash

cargo run --bin cairo-test --release -- corelib/ && \
cargo run --bin cairo-test --release -- tests/bug_samples/ --starknet
