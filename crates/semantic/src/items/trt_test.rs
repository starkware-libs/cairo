use crate::semantic_test;
use crate::test_utils::{test_function_diagnostics, SemanticDatabaseForTesting};

semantic_test!(diagnostics_tests, ["src/items/tests/trait"], test_function_diagnostics);
