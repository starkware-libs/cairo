use cairo_felt::Felt252;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeArg, AttributeArgVariant};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use cairo_lang_utils::OptionHelper;
use itertools::chain;
use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive;
use serde::{Deserialize, Serialize};

use super::{AVAILABLE_GAS_ATTR, IGNORE_ATTR, SHOULD_PANIC_ATTR, STATIC_GAS_ARG, TEST_ATTR};

pub const BYTE_ARRAY_MAGIC_HIGH: u128 = 0x46a6158a16a947e5916b2a2ca68501a;
pub const BYTE_ARRAY_MAGIC_LOW: u128 = 0x45e93d7110e81aa2d6438b1c57c879a3;

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
    let available_gas = extract_available_gas(available_gas_attr, db, &mut diagnostics);
    let (should_panic, expected_panic_felts) = if let Some(attr) = should_panic_attr {
        if attr.args.is_empty() {
            (true, None)
        } else {
            (
                true,
                extract_panic_values(db, attr).on_none(|| {
                    diagnostics.push(PluginDiagnostic {
                        stable_ptr: attr.args_stable_ptr.untyped(),
                        message: "Expected panic must be of the form `expected: <tuple of \
                                  felt252s>` or `expected: \"some string\"`."
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
    let mut add_malformed_attr_diag = || {
        diagnostics.push(PluginDiagnostic {
            stable_ptr: attr.args_stable_ptr.untyped(),
            message: format!(
                "Attribute should have a single numeric literal argument or `{STATIC_GAS_ARG}`."
            ),
        })
    };
    match &attr.args[..] {
        [
            AttributeArg {
                variant: AttributeArgVariant::Unnamed { value: ast::Expr::Literal(literal), .. },
                ..
            },
        ] => literal.numeric_value(db).and_then(|v| v.to_usize()).on_none(add_malformed_attr_diag),
        [
            AttributeArg {
                variant: AttributeArgVariant::Unnamed { value: ast::Expr::Path(path), .. },
                ..
            },
        ] if path.as_syntax_node().get_text_without_trivia(db) == STATIC_GAS_ARG => None,
        _ => {
            add_malformed_attr_diag();
            None
        }
    }
}

/// Tries to extract the relevant expected panic values.
fn extract_panic_values(db: &dyn SyntaxGroup, attr: &Attribute) -> Option<Vec<Felt252>> {
    let [AttributeArg { variant: AttributeArgVariant::Named { name, value, .. }, .. }] =
        &attr.args[..]
    else {
        return None;
    };
    if name != "expected" {
        return None;
    }
    match value {
        ast::Expr::Tuple(panic_bytes) => panic_bytes
            .expressions(db)
            .elements(db)
            .into_iter()
            .map(|value| match value {
                ast::Expr::Literal(literal) => {
                    Some(literal.numeric_value(db).unwrap_or_default().into())
                }
                ast::Expr::ShortString(literal) => {
                    Some(literal.numeric_value(db).unwrap_or_default().into())
                }
                _ => None,
            })
            .collect::<Option<Vec<_>>>(),
        ast::Expr::String(panic_string) => {
            let panic_string = panic_string.string_value(db).unwrap();
            let chunks = panic_string.as_bytes().chunks_exact(31);
            let data_len = chunks.len().into();
            let remainder = chunks.remainder();
            let pending_word_len = remainder.len().into();
            let mut data_felts = Vec::new();
            for chunk in chunks {
                let value = BigInt::from_bytes_be(Sign::Plus, chunk);
                data_felts.push(value.into());
            }
            let pending_word = BigInt::from_bytes_be(Sign::Plus, remainder).into();

            let byte_array_magic: BigInt =
                (BigInt::from(BYTE_ARRAY_MAGIC_HIGH) << 128) + BigInt::from(BYTE_ARRAY_MAGIC_LOW);
            Some(
                chain!(
                    [byte_array_magic.into(), data_len],
                    data_felts.into_iter(),
                    [pending_word, pending_word_len]
                )
                .collect(),
            )
        }
        _ => None,
    }
}
