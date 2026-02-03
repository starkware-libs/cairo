use std::slice;

use cairo_lang_semantic as semantic;
use itertools::Itertools;

use super::LoweredExpr;
use super::context::LoweringContext;
use crate::ids::LocationId;
use crate::{VarUsage, VariableId};

/// Given a return type of an external function, gets the real output variable types for that call.
/// For example, an external function that returns a tuple, has an output variable for each tuple
/// entry.
pub fn extern_facade_return_tys<'ret, 'db: 'ret>(
    db: &'db dyn salsa::Database,
    ret_ty: &'ret semantic::TypeId<'db>,
) -> &'ret [semantic::TypeId<'db>] {
    if let semantic::TypeLongId::Tuple(tys) = ret_ty.long(db) {
        tys
    } else {
        slice::from_ref(ret_ty)
    }
}

/// Given the returned output variables from an external function call, creates a LoweredExpr
/// representing the return expression of the type that was declared in the signature.
/// For example, for an external function that returns a tuple, even though it will have an output
/// variable for each entry, the return expression is a single value of type tuple.
pub fn extern_facade_expr<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    ty: semantic::TypeId<'db>,
    returns: impl ExactSizeIterator<Item = VariableId>,
    location: LocationId<'db>,
) -> LoweredExpr<'db> {
    if let semantic::TypeLongId::Tuple(subtypes) = ty.long(ctx.db) {
        assert_eq!(returns.len(), subtypes.len());
        // TODO(ilya): Use tuple item location for each item.
        LoweredExpr::Tuple {
            exprs: returns
                .map(|var_id| LoweredExpr::AtVariable(VarUsage { var_id, location }))
                .collect(),
            location,
        }
    } else {
        let Ok(var_id) = returns.exactly_one() else {
            panic!("Expected exactly one output variable for non-tuple return type");
        };
        LoweredExpr::AtVariable(VarUsage { var_id, location })
    }
}
