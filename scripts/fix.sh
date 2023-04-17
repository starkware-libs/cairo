#!/bin/bash

CAIRO_FIX_TESTS=1 cargo test --test test_full_contract_deserialization --test test_compile_path
