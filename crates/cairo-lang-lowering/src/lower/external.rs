use cairo_lang_semantic as semantic;

use super::LoweredExpr;
use super::context::LoweringContext;
use crate::ids::LocationId;
use crate::{VarUsage, VariableId};

/// Given a return type of an external function, gets the real output variable types for that call.
/// For example, an external function that returns a tuple, has an output variable for each tuple
/// entry.
pub fn extern_facade_return_tys<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    ret_ty: semantic::TypeId<'db>,
) -> Vec<semantic::TypeId<'db>> {
    if let semantic::TypeLongId::Tuple(tys) = ret_ty.long(ctx.db) {
        tys.to_vec()
    } else {
        vec![ret_ty]
    }
}

/// Given the returned output variables from an external function call, creates a LoweredExpr
/// representing the return expression of the type that was declared in the signature.
/// For example, for an external function that returns a tuple, even though it will have an output
/// variable for each entry, the return expression is a single value of type tuple.
pub fn extern_facade_expr<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    ty: semantic::TypeId<'db>,
    returns: Vec<VariableId>,
    location: LocationId<'db>,
) -> LoweredExpr<'db> {
    if let semantic::TypeLongId::Tuple(subtypes) = ty.long(ctx.db) {
        assert_eq!(returns.len(), subtypes.len());
        // TODO(ilya): Use tuple item location for each item.
        LoweredExpr::Tuple {
            exprs: returns
                .into_iter()
                .map(|var_id| LoweredExpr::AtVariable(VarUsage { var_id, location }))
                .collect(),
            location,
        }
    } else {
        assert_eq!(returns.len(), 1);
        LoweredExpr::AtVariable(VarUsage { var_id: returns.into_iter().next().unwrap(), location })
    }
}
