mod array_test;
mod bool_test;
mod box_test;
mod byte_array_test;
mod bytes31_test;
mod circuit_test;
mod clone_test;
mod cmp_test;
mod coupon_test;
mod deref_test;
mod dict_test;
mod ec_test;
mod felt_test;
mod fmt_test;
mod hash_test;
mod integer_test;
mod iter_test;
mod keccak_test;
mod math_test;
mod nullable_test;
mod num_test;
mod option_test;
mod plugins_test;
mod print_test;
mod qm31_test;
mod range_test;
mod result_test;
mod secp256k1_test;
mod secp256r1_test;
mod sha256_test;
mod test_utils;
mod testing_test;
mod to_byte_array_test;

/// Tests for language features, without mixing implementations within the corelib.
mod language_features {
    mod block_level_items_test;
    mod closure_test;
    mod const_folding_test;
    mod const_test;
    mod early_return_test;
    mod for_test;
    mod glob_use_test;
    mod panics_test;
    mod trait_test;
    mod while_test;
}
