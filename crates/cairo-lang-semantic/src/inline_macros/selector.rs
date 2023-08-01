use cairo_felt::Felt252;
use cairo_lang_defs::plugin::{InlineMacroPlugin, InlinePluginResult, PluginDiagnostic};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use num_bigint::BigUint;
use sha3::{Digest, Keccak256};

use super::unsupported_bracket_diagnostic;

#[derive(Debug)]
pub struct SelectorMacro;

// TODO: this is copied from cairo-lang-starknet due to circular dependency issue, how to fix?
/// A variant of eth-keccak that computes a value that fits in a Starknet field element.
pub fn starknet_keccak(data: &[u8]) -> BigUint {
    let mut hasher = Keccak256::new();
    hasher.update(data);
    let mut result = hasher.finalize();

    // Truncate result to 250 bits.
    *result.first_mut().unwrap() &= 3;
    BigUint::from_bytes_be(&result)
}

impl InlineMacroPlugin for SelectorMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> InlinePluginResult {
        let ast::WrappedExprList::ParenthesizedExprList(args) = syntax.arguments(db) else {
            return unsupported_bracket_diagnostic(db, syntax);
        };

        let arguments = &args.expressions(db).elements(db);
        if arguments.len() != 1 {
            let diagnostics = vec![PluginDiagnostic {
                stable_ptr: syntax.stable_ptr().untyped(),
                message: "selector macro must have a single argument".to_string(),
            }];
            return InlinePluginResult { code: None, diagnostics };
        }

        let ast::Expr::ShortString(short_string) = &arguments[0] else {
            let diagnostics = vec![PluginDiagnostic {
                stable_ptr: syntax.stable_ptr().untyped(),
                message: "selector macro argument must be a short string".to_string(),
            }];
            return InlinePluginResult { code: None, diagnostics };
        };
        let selector_bytes = short_string.numeric_value(db).unwrap().to_signed_bytes_be();

        let selector = Felt252::try_from(starknet_keccak(&selector_bytes)).unwrap();
        let code: String = format!("0x{}", selector.to_str_radix(16));
        InlinePluginResult { code: Some(code), diagnostics: vec![] }
    }
}
