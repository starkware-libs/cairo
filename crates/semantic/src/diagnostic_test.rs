use crate::diagnostics_test;
use crate::test_utils::{setup_test_expr, SemanticDatabaseForTesting};

diagnostics_test!(
    ["src/diagnostic_test_data/tests"],
    SemanticDatabaseForTesting::default(),
    setup_test_expr,
    "Expr Code",
    "Module Code",
    "Function Body"
);
