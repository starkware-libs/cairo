use crate::test_utils::test_function_diagnostics;

cairo_test_utils::test_file_test!(
    diagnostics,
    "src/items/tests",
    {
        enum_: "enum",
        extern_func: "extern_func",
        free_function: "free_function",
        panicable: "panicable",
        struct_: "struct",
        trait_: "trait",
        type_alias: "type_alias",
    },
    test_function_diagnostics
);
