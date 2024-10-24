use cairo_lang_semantic as semantic;
use cairo_lang_utils::LookupIntern;

use super::LoweredExpr;
use super::context::LoweringContext;
use crate::ids::LocationId;
use crate::{VarUsage, VariableId};

/// Given a return type of an external function, gets the real output variable types for that call.
/// For example, an external function that returns a tuple, has an output variable for each tuple
/// entry.
pub fn extern_facade_return_tys(
    ctx: &mut LoweringContext<'_, '_>,
    ret_ty: semantic::TypeId,
) -> Vec<semantic::TypeId> {
    if let semantic::TypeLongId::Tuple(tys) = ret_ty.lookup_intern(ctx.db) {
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
    ctx: &mut LoweringContext<'_, '_>,
    ty: semantic::TypeId,
    returns: Vec<VariableId>,
    location: LocationId,
) -> LoweredExpr {
    if let semantic::TypeLongId::Tuple(subtypes) = ty.lookup_intern(ctx.db) {
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
