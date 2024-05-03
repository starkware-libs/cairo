use cairo_lang_defs::ids::NamedLanguageElementId;
use cairo_lang_diagnostics::ToOption;
use cairo_lang_utils::{require, try_extract_matches};
use id_arena::Arena;
use num_bigint::BigInt;

use crate::corelib::{get_core_trait, CoreTraitContext};
use crate::db::SemanticGroup;
use crate::items::functions::GenericFunctionId;
use crate::{Expr, ExprFunctionCall, ExprFunctionCallArg};

/// If the given function call expression is a minus literal, extracts the literal. Otherwise,
/// returns None.
pub fn try_extract_minus_literal(
    db: &dyn SemanticGroup,
    exprs: &Arena<Expr>,
    expr: &ExprFunctionCall,
) -> Option<BigInt> {
    let [ExprFunctionCallArg::Value(expr_id)] = &expr.args[..] else {
        return None;
    };
    require(expr.coupon_arg.is_none())?;
    let literal = try_extract_matches!(&exprs[*expr_id], Expr::Literal)?;
    let imp = try_extract_matches!(
        expr.function.get_concrete(db).generic_function,
        GenericFunctionId::Impl
    )?;
    let trait_id = imp.impl_id.concrete_trait(db).to_option()?.trait_id(db);
    require(trait_id == get_core_trait(db, CoreTraitContext::TopLevel, "Neg".into()))?;
    if imp.function.name(db.upcast()) != "neg" { None } else { Some(-literal.value.clone()) }
}
