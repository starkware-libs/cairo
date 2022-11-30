use super::context::LoweringContext;
use super::variables::LivingVar;
use super::LoweredExpr;

/// Given a return type of an external function, gets the real output variable types for that call.
/// For example, an external function that returns a tuple, has an output variable for each tuple
/// entry.
pub fn extern_facade_return_tys(
    ctx: &mut LoweringContext<'_>,
    ret_ty: semantic::TypeId,
) -> Vec<semantic::TypeId> {
    if let semantic::TypeLongId::Tuple(tys) = ctx.db.lookup_intern_type(ret_ty) {
        tys
    } else {
        vec![ret_ty]
    }
}

/// Given the returned output variables from an external function call, creates a LoweredExpr
/// representing the return expression of the type that was declared in the signature.
/// For example, for an external function that returns a tuple, even though it will have an output
/// variable for each entry, the return expression is a single value of type tuple.
pub fn extern_facade_expr(
    ctx: &mut LoweringContext<'_>,
    ty: semantic::TypeId,
    returns: Vec<LivingVar>,
) -> LoweredExpr {
    if let semantic::TypeLongId::Tuple(subtypes) = ctx.db.lookup_intern_type(ty) {
        assert_eq!(returns.len(), subtypes.len());
        LoweredExpr::Tuple(returns.into_iter().map(LoweredExpr::AtVariable).collect())
    } else {
        assert_eq!(returns.len(), 1);
        LoweredExpr::AtVariable(returns.into_iter().next().unwrap())
    }
}
