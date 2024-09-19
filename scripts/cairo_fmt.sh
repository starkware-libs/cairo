#!/bin/bash

RUST_BACKTRACE=1 cargo run --profile=ci-dev --bin cairo-format -- -s --recursive "$@"
