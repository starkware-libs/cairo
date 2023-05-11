use std::sync::Arc;

use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, MacroPlugin, PluginGeneratedFile, PluginResult,
};
use cairo_lang_semantic::plugin::{AsDynMacroPlugin, SemanticPlugin, TrivialPluginAuxData};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use num_bigint::BigInt;

#[derive(Debug, Default)]
#[non_exhaustive]
pub struct ArithmeticPlugin;

impl MacroPlugin for ArithmeticPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        match item_ast {
            ast::Item::Constant(constant_ast) => handle_constant(db, &constant_ast),
            _ => PluginResult::default(),
        }
    }
}

impl AsDynMacroPlugin for ArithmeticPlugin {
    fn as_dyn_macro_plugin<'a>(self: Arc<Self>) -> Arc<dyn MacroPlugin + 'a>
    where
        Self: 'a,
    {
        self
    }
}
impl SemanticPlugin for ArithmeticPlugin {}

fn handle_constant(db: &dyn SyntaxGroup, constant_ast: &ast::ItemConstant) -> PluginResult {
    let value = constant_ast.value(db);
    if let ast::Expr::Literal(_) = value {
        return PluginResult::default();
    }
    let new_value = rec_compute(db, &value);
    if new_value.is_none() {
        return PluginResult::default();
    }
    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "computed_constants".into(),
            content: format!(
                "const {}{} = {};",
                constant_ast.name(db).text(db),
                constant_ast.type_clause(db).as_syntax_node().get_text(db),
                new_value.unwrap()
            ),
            aux_data: DynGeneratedFileAuxData(Arc::new(TrivialPluginAuxData {})),
        }),
        diagnostics: vec![],
        remove_original_item: true,
    }
}

// Don't print diagnostics, as downstream code will do it for us.
fn rec_compute(db: &dyn SyntaxGroup, value: &ast::Expr) -> Option<BigInt> {
    match value {
        ast::Expr::Literal(lit) => lit.numeric_value(db),
        ast::Expr::Binary(bin_expr) => match bin_expr.op(db) {
            ast::BinaryOperator::Plus(_) => {
                Some(rec_compute(db, &bin_expr.lhs(db))? + rec_compute(db, &bin_expr.rhs(db))?)
            }
            ast::BinaryOperator::Mul(_) => {
                Some(rec_compute(db, &bin_expr.lhs(db))? * rec_compute(db, &bin_expr.rhs(db))?)
            }
            ast::BinaryOperator::Minus(_) => {
                Some(rec_compute(db, &bin_expr.lhs(db))? - rec_compute(db, &bin_expr.rhs(db))?)
            }
            ast::BinaryOperator::Div(_) => {
                Some(rec_compute(db, &bin_expr.lhs(db))? / rec_compute(db, &bin_expr.rhs(db))?)
            }
            ast::BinaryOperator::Mod(_) => {
                Some(rec_compute(db, &bin_expr.lhs(db))? % rec_compute(db, &bin_expr.rhs(db))?)
            }
            ast::BinaryOperator::And(_) => {
                Some(rec_compute(db, &bin_expr.lhs(db))? & rec_compute(db, &bin_expr.rhs(db))?)
            }
            ast::BinaryOperator::Or(_) => {
                Some(rec_compute(db, &bin_expr.lhs(db))? | rec_compute(db, &bin_expr.rhs(db))?)
            }
            ast::BinaryOperator::Xor(_) => {
                Some(rec_compute(db, &bin_expr.lhs(db))? ^ rec_compute(db, &bin_expr.rhs(db))?)
            }
            _ => None,
        },
        ast::Expr::Unary(un_expr) => match un_expr.op(db) {
            ast::UnaryOperator::Minus(_) => Some(-rec_compute(db, &un_expr.expr(db))?),
            _ => None,
        },
        ast::Expr::Parenthesized(paren_expr) => rec_compute(db, &paren_expr.expr(db)),
        _ => None,
    }
}
