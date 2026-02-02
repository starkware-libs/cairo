use crate::test_utils::test_function_diagnostics;

cairo_lang_test_utils::test_file_test!(
    diagnostics,
    "src/items/tests",
    {
        enum_: "enum",
        extern_func: "extern_func",
        free_function: "free_function",
        impl_alias: "impl_alias",
        panicable: "panicable",
        struct_: "struct",
        trait_: "trait",
        trait_type: "trait_type",
        trait_const: "trait_const",
        trait_impl: "trait_impl",
        trait_default_fn: "trait_default_fn",
        type_alias: "type_alias",
        use_: "use_",
        module: "module",
        early_conform: "early_conform",
        type_mismatch_diagnostics: "type_mismatch_diagnostics",
    },
    test_function_diagnostics,
    ["expect_diagnostics"]
);
