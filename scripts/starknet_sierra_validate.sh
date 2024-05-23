#!/bin/bash

if [ -z "$1" ]; then
  N_BLOCKS=1000
else
  N_BLOCKS=$1
fi

cargo run --bin starknet-sierra-upgrade-validate -- --fullnode-url 'https://papyrus-mainnet-dan.sw-dev.io/rpc/v0_7_0' --last-n-blocks $N_BLOCKS