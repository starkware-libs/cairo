#!/bin/bash

cargo run --bin cairo-test -- corelib/ && \
cargo run --bin cairo-test -- tests/bug_samples/ --starknet
