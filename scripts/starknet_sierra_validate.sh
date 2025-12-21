#!/bin/bash

export FULLNODE_URL="${1:-https://starknet-mainnet.core.chainstack.com/57088c25afa0ff21277c6ee5f3b536bb/rpc/v0_7}"
export N_BLOCKS="${2:-1000}"

cargo run --bin starknet-sierra-upgrade-validate --profile=release -- --fullnode-url $FULLNODE_URL --last-n-blocks $N_BLOCKS
