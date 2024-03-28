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
        // TODO(yg): remove.
        trait_type2: "trait_type2",
        trait_type3: "trait_type3",
        type_alias: "type_alias",
        use_: "use_",
        module: "module",
    },
    test_function_diagnostics
);
