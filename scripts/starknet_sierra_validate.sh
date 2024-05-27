#!/bin/bash

export N_BLOCKS="${1:-1000}"

cargo run --bin starknet-sierra-upgrade-validate -- --fullnode-url 'https://papyrus-mainnet-dan.sw-dev.io/rpc/v0_7_0' --last-n-blocks $N_BLOCKS
