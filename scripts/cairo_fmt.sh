#!/bin/bash

cargo run --bin cairo-format --features="binary" -- --recursive "$@"
