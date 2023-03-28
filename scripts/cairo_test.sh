#!/bin/bash

cargo +nightly-2022-11-03 run --bin cairo-test -- corelib/ && \
cargo +nightly-2022-11-03 run --bin cairo-test -- tests/bug_samples/ --starknet
