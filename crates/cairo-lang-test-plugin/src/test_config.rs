use cairo_felt::{felt_str, Felt252};
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeArg, AttributeArgVariant};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::byte_array::{BYTES_IN_WORD, BYTE_ARRAY_MAGIC};
use cairo_lang_utils::OptionHelper;
use itertools::chain;
use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive;
use serde::{Deserialize, Serialize};

use super::{AVAILABLE_GAS_ATTR, IGNORE_ATTR, SHOULD_PANIC_ATTR, STATIC_GAS_ARG, TEST_ATTR};

/// Expectation for a panic case.
#[derive(Clone, Serialize, Deserialize, Debug, PartialEq)]
pub enum PanicExpectation {
    /// Accept any panic value.
    Any,
    /// Accept only a panic with this specific vector of felts.
    Exact(Vec<Felt252>),
}

/// Expectation for a result of a test.
#[derive(Clone, Serialize, Deserialize, Debug, PartialEq)]
pub enum TestExpectation {
    /// Running the test should not panic.
    Success,
    /// Running the test should result in a panic.
    Panics(PanicExpectation),
}

/// The configuration for running a single test.
#[derive(Clone, Serialize, Deserialize, Debug, PartialEq)]
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
    let test_attr = attrs.iter().find(|attr| attr.id.as_str() == TEST_ATTR);
    let ignore_attr = attrs.iter().find(|attr| attr.id.as_str() == IGNORE_ATTR);
    let available_gas_attr = attrs.iter().find(|attr| attr.id.as_str() == AVAILABLE_GAS_ATTR);
    let should_panic_attr = attrs.iter().find(|attr| attr.id.as_str() == SHOULD_PANIC_ATTR);
    let mut diagnostics = vec![];
    if let Some(attr) = test_attr {
        if !attr.args.is_empty() {
            diagnostics.push(PluginDiagnostic::error(
                attr.id_stable_ptr.untyped(),
                "Attribute should not have arguments.".into(),
            ));
        }
    } else {
        for attr in [ignore_attr, available_gas_attr, should_panic_attr].into_iter().flatten() {
            diagnostics.push(PluginDiagnostic::error(
                attr.id_stable_ptr.untyped(),
                "Attribute should only appear on tests.".into(),
            ));
        }
    }
    let ignored = if let Some(attr) = ignore_attr {
        if !attr.args.is_empty() {
            diagnostics.push(PluginDiagnostic::error(
                attr.id_stable_ptr.untyped(),
                "Attribute should not have arguments.".into(),
            ));
        }
        true
    } else {
        false
    };
    let available_gas = extract_available_gas(available_gas_attr, db, &mut diagnostics);
    let (should_panic, expected_panic_felts) = if let Some(attr) = should_panic_attr {
        if attr.args.is_empty() {
            (true, None)
        } else {
            (
                true,
                extract_panic_bytes(db, attr).on_none(|| {
                    diagnostics.push(PluginDiagnostic::error(
                        attr.args_stable_ptr.untyped(),
                        "Expected panic must be of the form `expected: <tuple of felt252s and \
                         strings>` or `expected: \"some string\"` or `expected: <some felt252>`."
                            .into(),
                    ));
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
                TestExpectation::Panics(if let Some(felts) = expected_panic_felts {
                    PanicExpectation::Exact(felts)
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

/// Extract the available gas from the attribute.
/// Adds a diagnostic if the attribute is malformed.
/// Returns `None` if the attribute is "static", or the attribute is malformed.
fn extract_available_gas(
    available_gas_attr: Option<&Attribute>,
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<usize> {
    let Some(attr) = available_gas_attr else {
        // If no gas is specified, we assume the reasonably large possible gas, such that infinite
        // loops will run out of gas.
        return Some(u32::MAX as usize);
    };
    match &attr.args[..] {
        [AttributeArg { variant: AttributeArgVariant::Unnamed { value, .. }, .. }] => match value {
            ast::Expr::Path(path)
                if path.as_syntax_node().get_text_without_trivia(db) == STATIC_GAS_ARG =>
            {
                return None;
            }
            ast::Expr::Literal(literal) => {
                literal.numeric_value(db).and_then(|v| v.to_i64()).and_then(|v| v.to_usize())
            }
            _ => None,
        },
        _ => None,
    }
    .on_none(|| {
        diagnostics.push(PluginDiagnostic::error(
            attr.args_stable_ptr.untyped(),
            format!(
                "Attribute should have a single non-negative literal in `i64` range or \
                 `{STATIC_GAS_ARG}`."
            ),
        ))
    })
}

/// Tries to extract the expected panic bytes out of the given `should_panic` attribute.
/// Assumes the attribute is `should_panic`.
fn extract_panic_bytes(db: &dyn SyntaxGroup, attr: &Attribute) -> Option<Vec<Felt252>> {
    let [AttributeArg { variant: AttributeArgVariant::Named { name, value, .. }, .. }] =
        &attr.args[..]
    else {
        return None;
    };
    if name != "expected" {
        return None;
    }

    match value {
        ast::Expr::Tuple(panic_exprs) => {
            let mut panic_bytes = Vec::new();
            for panic_expr in panic_exprs.expressions(db).elements(db) {
                match panic_expr {
                    ast::Expr::Literal(panic_expr) => {
                        panic_bytes.push(panic_expr.numeric_value(db).unwrap_or_default().into())
                    }
                    ast::Expr::ShortString(panic_expr) => {
                        panic_bytes.push(panic_expr.numeric_value(db).unwrap_or_default().into())
                    }
                    ast::Expr::String(panic_expr) => {
                        panic_bytes.append(&mut extract_string_panic_bytes(&panic_expr, db))
                    }
                    _ => return None,
                }
            }
            Some(panic_bytes)
        }
        ast::Expr::String(panic_string) => Some(extract_string_panic_bytes(panic_string, db)),
        ast::Expr::Literal(panic_expr) => {
            Some(vec![panic_expr.numeric_value(db).unwrap_or_default().into()])
        }
        ast::Expr::ShortString(panic_expr) => {
            Some(vec![panic_expr.numeric_value(db).unwrap_or_default().into()])
        }
        _ => None,
    }
}

/// Extracts panic bytes from a string.
fn extract_string_panic_bytes(
    panic_string: &ast::TerminalString,
    db: &dyn SyntaxGroup,
) -> Vec<Felt252> {
    let panic_string = panic_string.string_value(db).unwrap();
    let chunks = panic_string.as_bytes().chunks_exact(BYTES_IN_WORD);
    let num_full_words = chunks.len().into();
    let remainder = chunks.remainder();
    let pending_word_len = remainder.len().into();
    let full_words = chunks.map(|chunk| BigInt::from_bytes_be(Sign::Plus, chunk).into());
    let pending_word = BigInt::from_bytes_be(Sign::Plus, remainder).into();

    chain!(
        [felt_str!(BYTE_ARRAY_MAGIC, 16), num_full_words],
        full_words.into_iter(),
        [pending_word, pending_word_len]
    )
    .collect()
}
