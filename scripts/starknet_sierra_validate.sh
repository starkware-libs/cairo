#!/bin/bash

export FULLNODE_URL="${1:-https://papyrus-integration-mainnet.sw-dev.io/rpc/v0_7}"
export N_BLOCKS="${2:-1000}"

cargo run --bin starknet-sierra-upgrade-validate -- --fullnode-url "$FULLNODE_URL" --last-n-blocks "$N_BLOCKS"
