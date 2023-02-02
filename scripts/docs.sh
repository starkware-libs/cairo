#!/bin/bash

RUSTDOCFLAGS="-Dwarnings" cargo doc --document-private-items --no-deps --all-features
