use super::context::LoweringContext;
use super::variables::LivingVar;
use super::LoweredExpr;

pub fn extern_facade_tys(
    ctx: &mut LoweringContext<'_>,
    ty: semantic::TypeId,
) -> Vec<semantic::TypeId> {
    if let semantic::TypeLongId::Tuple(tys) = ctx.db.lookup_intern_type(ty) {
        tys
    } else {
        vec![ty]
    }
}

pub fn extern_facade_expr(
    ctx: &mut LoweringContext<'_>,
    ty: semantic::TypeId,
    returns: Vec<LivingVar>,
) -> LoweredExpr {
    if let semantic::TypeLongId::Tuple(_) = ctx.db.lookup_intern_type(ty) {
        LoweredExpr::Tuple(returns.into_iter().map(LoweredExpr::AtVariable).collect())
    } else {
        assert_eq!(returns.len(), 1);
        LoweredExpr::AtVariable(returns.into_iter().next().unwrap())
    }
}
