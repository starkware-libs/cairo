use crate::semantic_test;
use crate::test_utils::{test_expr_diagnostics, SemanticDatabaseForTesting};

semantic_test!(diagnostics_tests, ["src/diagnostic_test_data/tests"], test_expr_diagnostics);
