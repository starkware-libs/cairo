#!/bin/bash

cargo run --profile=ci-dev --bin cairo-format -- -s --recursive "$@" corelib/src/array.cairo
