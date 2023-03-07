#!/bin/bash

exit_code=0
cargo +nightly-2022-11-03 run --bin cairo-test -- --path corelib/
(( exit_code = exit_code || $? ))
cargo +nightly-2022-11-03 run --bin cairo-test -- --path tests/bug_samples/ --starknet
(( exit_code = exit_code || $? ))
exit $exit_code
