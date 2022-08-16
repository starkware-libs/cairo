use defs::db::DefsDatabase;
use pretty_assertions::assert_eq;
use salsa::{InternId, InternKey};
use semantic::db::{SemanticDatabase, SemanticGroup};
use semantic::ids::TypeId;

use crate::expr_generator::generate_expression_code;
use crate::expr_generator_context::{ExprGeneratorContext, SierraVariable};

#[salsa::database(DefsDatabase, SemanticDatabase)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl salsa::Database for DatabaseImpl {}

#[test]
fn test_expr_generator() {
    let db = DatabaseImpl::default();

    let ty = TypeId::from_intern_id(InternId::from(0u32));
    let literal7 = db.expr(semantic::Expr::ExprLiteral(semantic::ExprLiteral { value: 7, ty }));

    let mut expr_generator_context = ExprGeneratorContext::new(&db);
    let (instructions, res) = generate_expression_code(&mut expr_generator_context, literal7);
    assert_eq!(instructions, vec!["literal<7>() -> (var0);",]);

    assert_eq!(res, SierraVariable::from(0));
}
