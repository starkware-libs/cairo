#!/bin/bash

cargo run --profile=ci-dev --bin cairo-format -- --recursive "$@"
