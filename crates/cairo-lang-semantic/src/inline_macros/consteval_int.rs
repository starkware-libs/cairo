use cairo_lang_defs::extract_macro_single_unnamed_arg;
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, MacroPluginMetadata, NamedPlugin, PluginDiagnostic,
    PluginGeneratedFile,
};
use cairo_lang_defs::plugin_utils::{PluginResultTrait, not_legacy_macro_diagnostic};
use cairo_lang_filesystem::ids::{CodeMapping, CodeOrigin};
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_parser::macro_helpers::AsLegacyInlineMacro;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode, ast};
use num_bigint::BigInt;

#[derive(Debug, Default)]
pub struct ConstevalIntMacro;
impl NamedPlugin for ConstevalIntMacro {
    const NAME: &'static str = "consteval_int";
}
impl InlineMacroExprPlugin for ConstevalIntMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
        metadata: &MacroPluginMetadata<'_>,
    ) -> InlinePluginResult {
        let Some(legacy_inline_macro) = syntax.clone().as_legacy_inline_macro(db) else {
            return InlinePluginResult::diagnostic_only(not_legacy_macro_diagnostic(
                syntax.as_syntax_node().stable_ptr(),
            ));
        };
        let constant_expression = extract_macro_single_unnamed_arg!(
            db,
            &legacy_inline_macro,
            ast::WrappedArgList::ParenthesizedArgList(_),
            syntax.stable_ptr()
        );

        let mut diagnostics = vec![];
        const DEPRECATION_FEATURE: &str = r#""deprecated-consteval-int-macro""#;
        if !metadata.allowed_features.contains(DEPRECATION_FEATURE) {
            diagnostics.push(PluginDiagnostic::warning(
                syntax.stable_ptr().untyped(),
                format!(
                    "Usage of deprecated macro `{}` with no `#[feature({DEPRECATION_FEATURE})]` \
                     attribute. Note: Use simple calculations instead, as these are supported in \
                     const context.",
                    Self::NAME
                ),
            ));
        }
        let code = compute_constant_expr(db, &constant_expression, &mut diagnostics, syntax);
        InlinePluginResult {
            code: code.map(|x| {
                let content = x.to_string();
                let span = TextSpan {
                    start: TextOffset::default(),
                    end: TextOffset::default().add_width(TextWidth::from_str(&content)),
                };
                PluginGeneratedFile {
                    name: "consteval_int_inline_macro".into(),
                    content,
                    code_mappings: vec![CodeMapping {
                        span,
                        origin: CodeOrigin::Span(syntax.as_syntax_node().span(db)),
                    }],
                    aux_data: None,
                }
            }),
            diagnostics,
        }
    }
}

/// Compute the actual value of an integer expression, or fail with diagnostics.
/// This computation handles arbitrary integers, unlike regular Cairo math.
pub fn compute_constant_expr(
    db: &dyn SyntaxGroup,
    value: &ast::Expr,
    diagnostics: &mut Vec<PluginDiagnostic>,
    macro_ast: &ast::ExprInlineMacro,
) -> Option<BigInt> {
    match value {
        ast::Expr::Literal(lit) => lit.numeric_value(db),
        ast::Expr::Binary(bin_expr) => match bin_expr.op(db) {
            ast::BinaryOperator::Plus(_) => Some(
                compute_constant_expr(db, &bin_expr.lhs(db), diagnostics, macro_ast)?
                    + compute_constant_expr(db, &bin_expr.rhs(db), diagnostics, macro_ast)?,
            ),
            ast::BinaryOperator::Mul(_) => Some(
                compute_constant_expr(db, &bin_expr.lhs(db), diagnostics, macro_ast)?
                    * compute_constant_expr(db, &bin_expr.rhs(db), diagnostics, macro_ast)?,
            ),
            ast::BinaryOperator::Minus(_) => Some(
                compute_constant_expr(db, &bin_expr.lhs(db), diagnostics, macro_ast)?
                    - compute_constant_expr(db, &bin_expr.rhs(db), diagnostics, macro_ast)?,
            ),
            ast::BinaryOperator::Div(_) => Some(
                compute_constant_expr(db, &bin_expr.lhs(db), diagnostics, macro_ast)?
                    / compute_constant_expr(db, &bin_expr.rhs(db), diagnostics, macro_ast)?,
            ),
            ast::BinaryOperator::Mod(_) => Some(
                compute_constant_expr(db, &bin_expr.lhs(db), diagnostics, macro_ast)?
                    % compute_constant_expr(db, &bin_expr.rhs(db), diagnostics, macro_ast)?,
            ),
            ast::BinaryOperator::And(_) => Some(
                compute_constant_expr(db, &bin_expr.lhs(db), diagnostics, macro_ast)?
                    & compute_constant_expr(db, &bin_expr.rhs(db), diagnostics, macro_ast)?,
            ),
            ast::BinaryOperator::Or(_) => Some(
                compute_constant_expr(db, &bin_expr.lhs(db), diagnostics, macro_ast)?
                    | compute_constant_expr(db, &bin_expr.rhs(db), diagnostics, macro_ast)?,
            ),
            ast::BinaryOperator::Xor(_) => Some(
                compute_constant_expr(db, &bin_expr.lhs(db), diagnostics, macro_ast)?
                    ^ compute_constant_expr(db, &bin_expr.rhs(db), diagnostics, macro_ast)?,
            ),
            _ => {
                diagnostics.push(PluginDiagnostic::error_with_inner_span(
                    db,
                    macro_ast.stable_ptr(),
                    bin_expr.as_syntax_node(),
                    "Unsupported binary operator in consteval_int macro".to_string(),
                ));
                None
            }
        },
        ast::Expr::Unary(un_expr) => match un_expr.op(db) {
            ast::UnaryOperator::Minus(_) => {
                Some(-compute_constant_expr(db, &un_expr.expr(db), diagnostics, macro_ast)?)
            }
            _ => {
                diagnostics.push(PluginDiagnostic::error_with_inner_span(
                    db,
                    macro_ast.stable_ptr(),
                    un_expr.as_syntax_node(),
                    "Unsupported unary operator in consteval_int macro".to_string(),
                ));
                None
            }
        },
        ast::Expr::Parenthesized(paren_expr) => {
            compute_constant_expr(db, &paren_expr.expr(db), diagnostics, macro_ast)
        }
        _ => {
            diagnostics.push(PluginDiagnostic::error_with_inner_span(
                db,
                macro_ast.stable_ptr(),
                value.as_syntax_node(),
                "Unsupported expression in consteval_int macro".to_string(),
            ));
            None
        }
    }
}
