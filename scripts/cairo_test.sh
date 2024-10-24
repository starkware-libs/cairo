#!/bin/bash

cargo run --profile=ci-dev --bin cairo-test -- corelib/ &&
    cargo run --profile=ci-dev --bin cairo-test -- tests/bug_samples/ --starknet
