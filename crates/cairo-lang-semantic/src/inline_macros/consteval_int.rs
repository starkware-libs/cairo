use cairo_lang_defs::extract_macro_single_unnamed_arg;
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, MacroPluginMetadata, NamedPlugin, PluginDiagnostic,
    PluginGeneratedFile,
};
use cairo_lang_defs::plugin_utils::{PluginResultTrait, not_legacy_macro_diagnostic};
use cairo_lang_filesystem::ids::{CodeMapping, CodeOrigin, SmolStrId};
use cairo_lang_filesystem::span::TextSpan;
use cairo_lang_parser::macro_helpers::AsLegacyInlineMacro;
use cairo_lang_syntax::node::{TypedSyntaxNode, ast};
use indoc::indoc;
use num_bigint::BigInt;
use salsa::Database;

#[derive(Debug, Default)]
pub struct ConstevalIntMacro;
impl NamedPlugin for ConstevalIntMacro {
    const NAME: &'static str = "consteval_int";
}
impl InlineMacroExprPlugin for ConstevalIntMacro {
    fn generate_code<'db>(
        &self,
        db: &'db dyn Database,
        syntax: &ast::ExprInlineMacro<'db>,
        metadata: &MacroPluginMetadata<'_>,
    ) -> InlinePluginResult<'db> {
        let Some(legacy_inline_macro) = syntax.as_legacy_inline_macro(db) else {
            return InlinePluginResult::diagnostic_only(not_legacy_macro_diagnostic(
                syntax.as_syntax_node().stable_ptr(db),
            ));
        };
        let constant_expression = extract_macro_single_unnamed_arg!(
            db,
            &legacy_inline_macro,
            ast::WrappedArgList::ParenthesizedArgList(_),
            syntax.stable_ptr(db)
        );

        let mut diagnostics = vec![];
        let deprecation_feature = SmolStrId::from(db, r#""deprecated-consteval-int-macro""#);
        if !metadata.allowed_features.contains(&deprecation_feature) {
            diagnostics.push(PluginDiagnostic::warning(
                syntax.stable_ptr(db),
                format!(
                    "Usage of deprecated macro `{}` with no `#[feature({})]` attribute. Note: Use \
                     simple calculations instead, as these are supported in const context.",
                    Self::NAME,
                    deprecation_feature.long(db),
                ),
            ));
        }
        let code = compute_constant_expr(db, &constant_expression, &mut diagnostics, syntax);
        InlinePluginResult {
            code: code.map(|x| {
                let content = x.to_string();
                let span = TextSpan::from_str(&content);
                PluginGeneratedFile {
                    name: "consteval_int_inline_macro".into(),
                    content,
                    code_mappings: vec![CodeMapping {
                        span,
                        origin: CodeOrigin::Span(syntax.as_syntax_node().span(db)),
                    }],
                    aux_data: None,
                    diagnostics_note: Default::default(),
                    is_unhygienic: false,
                }
            }),
            diagnostics,
        }
    }

    fn documentation(&self) -> Option<String> {
        Some(
            indoc! {r#"
            Evaluates an integer expression at compile time.
            The `consteval_int!` macro computes an integer expression \
            during compilation and replaces itself with the computed value.
            This macro is deprecated; use const expressions directly instead.

            # Syntax
            ```cairo
            consteval_int!(expression)
            ```
            # Parameters
            - `expression`: An integer expression to evaluate at compile time.

            # Examples
            ```cairo
            let x = consteval_int!(2 + 3); // Equivalent to: let x = 5;
            let y = consteval_int!(4 * 5); // Equivalent to: let y = 20;
            ```
            "#}
            .to_string(),
        )
    }
}

/// Compute the actual value of an integer expression, or fail with diagnostics.
/// This computation handles arbitrary integers, unlike regular Cairo math.
pub fn compute_constant_expr<'db>(
    db: &'db dyn Database,
    value: &ast::Expr<'db>,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
    macro_ast: &ast::ExprInlineMacro<'db>,
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
                    macro_ast.stable_ptr(db),
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
                    macro_ast.stable_ptr(db),
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
                macro_ast.stable_ptr(db),
                value.as_syntax_node(),
                "Unsupported expression in consteval_int macro".to_string(),
            ));
            None
        }
    }
}
