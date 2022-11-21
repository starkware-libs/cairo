use pretty_assertions::assert_eq;

use crate::semantic_test;
use crate::test_utils::{test_function_diagnostics, SemanticDatabaseForTesting};

semantic_test!(enum_diagnostics_tests, ["src/items/tests/enum"], test_function_diagnostics);
semantic_test!(
    extern_func_diagnostics_tests,
    ["src/items/tests/extern_func"],
    test_function_diagnostics
);
semantic_test!(
    free_function_diagnostics_tests,
    ["src/items/tests/free_function"],
    test_function_diagnostics
);
semantic_test!(
    panicable_diagnostics_tests,
    ["src/items/tests/panicable"],
    test_function_diagnostics
);
semantic_test!(struct_diagnostics_tests, ["src/items/tests/struct"], test_function_diagnostics);
semantic_test!(trait_diagnostics_tests, ["src/items/tests/trait"], test_function_diagnostics);
