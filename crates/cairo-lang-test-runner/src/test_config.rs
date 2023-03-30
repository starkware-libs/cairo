use cairo_felt::Felt252;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::items::attribute::Attribute;
use cairo_lang_semantic::literals::LiteralLongId;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal, Token, TypedSyntaxNode};
use cairo_lang_utils::OptionHelper;
use unescaper::unescape;

/// Expectation for a panic case.
pub enum PanicExpectation {
    /// Accept any panic value.
    Any,
    /// Accept only this specific vector of panics.
    Exact(Vec<Felt252>),
}

/// Expectation for a result of a test.
pub enum TestExpectation {
    /// Running the test should not panic.
    Success,
    /// Running the test should result in a panic.
    Panics(PanicExpectation),
}

/// The configuration for running a single test.
pub struct TestConfig {
    /// The amount of gas the test requested.
    pub available_gas: Option<usize>,
    /// The expected result of the run.
    pub expectation: TestExpectation,
    /// Should the test be ignored.
    pub ignored: bool,
}

/// Extracts the configuration of a tests from attributes, or returns the diagnostics if the
/// attributes are set illegally.
pub fn try_extract_test_config(
    db: &dyn SyntaxGroup,
    attrs: Vec<Attribute>,
) -> Result<Option<TestConfig>, Vec<PluginDiagnostic>> {
    let test_attr = attrs.iter().find(|attr| attr.id.as_str() == "test");
    let ignore_attr = attrs.iter().find(|attr| attr.id.as_str() == "ignore");
    let available_gas_attr = attrs.iter().find(|attr| attr.id.as_str() == "available_gas");
    let should_panic_attr = attrs.iter().find(|attr| attr.id.as_str() == "should_panic");
    let mut diagnostics = vec![];
    if let Some(attr) = test_attr {
        if !attr.args.is_empty() {
            diagnostics.push(PluginDiagnostic {
                stable_ptr: attr.id_stable_ptr.untyped(),
                message: "Attribute should not have arguments.".into(),
            });
        }
    } else {
        for attr in [ignore_attr, available_gas_attr, should_panic_attr].into_iter().flatten() {
            diagnostics.push(PluginDiagnostic {
                stable_ptr: attr.id_stable_ptr.untyped(),
                message: "Attribute should only appear on tests.".into(),
            });
        }
    }
    let ignored = if let Some(attr) = ignore_attr {
        if !attr.args.is_empty() {
            diagnostics.push(PluginDiagnostic {
                stable_ptr: attr.id_stable_ptr.untyped(),
                message: "Attribute should not have arguments.".into(),
            });
        }
        true
    } else {
        false
    };
    let available_gas = if let Some(attr) = available_gas_attr {
        if let [ast::Expr::Literal(literal)] = &attr.args[..] {
            literal.token(db).text(db).parse::<usize>().ok()
        } else {
            diagnostics.push(PluginDiagnostic {
                stable_ptr: attr.id_stable_ptr.untyped(),
                message: "Attribute should have a single value argument.".into(),
            });
            None
        }
    } else {
        None
    };
    let (should_panic, expected_panic_value) = if let Some(attr) = should_panic_attr {
        if attr.args.is_empty() {
            (true, None)
        } else {
            (
                true,
                extract_panic_values(db, attr).on_none(|| {
                    diagnostics.push(PluginDiagnostic {
                        stable_ptr: attr.args_stable_ptr.untyped(),
                        message: "Expected panic must be of the form `expected = <tuple of \
                                  felt252s>`."
                            .into(),
                    });
                }),
            )
        }
    } else {
        (false, None)
    };
    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }
    Ok(if test_attr.is_none() {
        None
    } else {
        Some(TestConfig {
            available_gas,
            expectation: if should_panic {
                TestExpectation::Panics(if let Some(values) = expected_panic_value {
                    PanicExpectation::Exact(values)
                } else {
                    PanicExpectation::Any
                })
            } else {
                TestExpectation::Success
            },
            ignored,
        })
    })
}

/// Tries to extract the relevant expected panic values.
fn extract_panic_values(db: &dyn SyntaxGroup, attr: &Attribute) -> Option<Vec<Felt252>> {
    let [ast::Expr::Binary(binary)] = &attr.args[..] else { return None; };
    if !matches!(binary.op(db), ast::BinaryOperator::Eq(_)) {
        return None;
    }
    if binary.lhs(db).as_syntax_node().get_text_without_trivia(db) != "expected" {
        return None;
    }
    let ast::Expr::Tuple(panics) = binary.rhs(db) else { return None };
    panics
        .expressions(db)
        .elements(db)
        .into_iter()
        .map(|value| match value {
            ast::Expr::Literal(literal) => {
                Felt252::try_from(LiteralLongId::try_from(literal.token(db).text(db)).ok()?.value)
                    .ok()
            }
            ast::Expr::ShortString(short_string_syntax) => {
                let text = short_string_syntax.text(db);
                let (literal, _) = text[1..].rsplit_once('\'')?;
                let unescaped_literal = unescape(literal).ok()?;
                Some(Felt252::from_bytes_be(unescaped_literal.as_bytes()))
            }
            _ => None,
        })
        .collect::<Option<Vec<_>>>()
}
