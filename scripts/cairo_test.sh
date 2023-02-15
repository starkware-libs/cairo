#!/bin/bash

cargo +nightly-2022-11-03 run --bin cairo-test -- --path corelib/
cargo +nightly-2022-11-03 run --bin cairo-test -- --path tests/bug_samples/
