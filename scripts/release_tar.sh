#!/bin/bash

set -ex

NAMES="cairo-compile cairo-format cairo-run cairo-execute cairo-test sierra-compile starknet-compile starknet-sierra-compile"
TARGET=$1
rustup target add $TARGET
cargo build --release --target $TARGET
rm -rf target/$TARGET/cairo
rm -rf target/$TARGET.tar.gz
mkdir -p target/$TARGET/cairo
(
    cd target/$TARGET
    mkdir cairo/bin
    for NAME in $NAMES; do
        cp release/$NAME cairo/bin/
    done
    cp -R ../../corelib cairo/
    tar czvf ../$TARGET.tar.gz cairo
)
