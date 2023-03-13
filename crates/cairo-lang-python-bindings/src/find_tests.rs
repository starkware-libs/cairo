use cairo_lang_defs::ids::{FreeFunctionId, FunctionWithBodyId, ModuleItemId};
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_syntax::node::ast::Expr;
use cairo_lang_syntax::node::Token;

/// Expectation for a result of a test.
pub enum TestExpectation {
    /// Running the test should not panic.
    Success,
    /// Running the test should result in a panic.
    Panics,
}

/// The configuration for running a single test.
#[allow(dead_code)]
pub struct TestConfig {
    /// The function id of the test function.
    pub func_id: FreeFunctionId,
    /// The amount of gas the test requested.
    pub available_gas: Option<usize>,
    /// The expected result of the run.
    pub expectation: TestExpectation,
    /// Should the test be ignored.
    pub ignored: bool,
}

/// Finds the tests in the requested crates.
pub fn find_all_tests(db: &dyn SemanticGroup, main_crates: Vec<CrateId>) -> Vec<TestConfig> {
    let mut tests = vec![];
    for crate_id in main_crates {
        let modules = db.crate_modules(crate_id);
        for module_id in modules.iter() {
            let Ok(module_items) = db.module_items(*module_id) else {
              continue;
          };

            for item in module_items.iter() {
                if let ModuleItemId::FreeFunction(func_id) = item {
                    if let Ok(attrs) =
                        db.function_with_body_attributes(FunctionWithBodyId::Free(*func_id))
                    {
                        let mut is_test = false;
                        let mut available_gas = None;
                        let mut ignored = false;
                        let mut should_panic = false;
                        for attr in attrs {
                            match attr.id.as_str() {
                                "test" => {
                                    is_test = true;
                                }
                                "available_gas" => {
                                    // TODO(orizi): Provide diagnostics when this does not match.
                                    if let [Expr::Literal(literal)] = &attr.args[..] {
                                        available_gas = literal
                                            .token(db.upcast())
                                            .text(db.upcast())
                                            .parse::<usize>()
                                            .ok();
                                    }
                                }
                                "should_panic" => {
                                    should_panic = true;
                                }
                                "ignore" => {
                                    ignored = true;
                                }
                                _ => {}
                            }
                        }
                        if is_test {
                            tests.push(TestConfig {
                                func_id: *func_id,
                                available_gas,
                                expectation: if should_panic {
                                    TestExpectation::Panics
                                } else {
                                    TestExpectation::Success
                                },
                                ignored,
                            })
                        }
                    }
                }
            }
        }
    }
    tests
}
