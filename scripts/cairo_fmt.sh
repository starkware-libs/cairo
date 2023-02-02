#!/bin/bash

cargo +nightly-2022-11-03 run --bin cairo-format -- --recursive "$@"
