use std::cmp::Ordering;

use cairo_lang_defs::ids::{FunctionWithBodyId, UnstableSalsaId};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use cairo_lang_utils::graph_algos::graph_node::GraphNode;
use cairo_lang_utils::graph_algos::strongly_connected_components::compute_scc;
use semantic::items::functions::GenericFunctionId;

use crate::db::{LoweringGroup, SCCRepresentative};

// TODO(yuval): remove after moving panic phase to after monomorphization.
/// Query implementation of [crate::db::LoweringGroup::function_scc_representative].
pub fn function_scc_representative(
    db: &dyn LoweringGroup,
    function: FunctionWithBodyId,
) -> SCCRepresentative {
    SCCRepresentative(
        db.function_with_body_scc(function)
            .into_iter()
            .min_by(|x, y| match (x, y) {
                (FunctionWithBodyId::Free(x), FunctionWithBodyId::Free(y)) => {
                    x.get_internal_id().cmp(y.get_internal_id())
                }
                (FunctionWithBodyId::Impl(x), FunctionWithBodyId::Impl(y)) => {
                    x.get_internal_id().cmp(y.get_internal_id())
                }
                (FunctionWithBodyId::Free(_), FunctionWithBodyId::Impl(_)) => Ordering::Less,
                (FunctionWithBodyId::Impl(_), FunctionWithBodyId::Free(_)) => Ordering::Greater,
            })
            .unwrap_or(function),
    )
}

// TODO(yuval): remove after moving panic phase to after monomorphization.
/// Query implementation of [crate::db::LoweringGroup::function_with_body_scc].
pub fn function_with_body_scc(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
) -> Vec<FunctionWithBodyId> {
    compute_scc(&FunctionWithBodyNode { function_with_body_id: function_id, db: db.upcast() })
}

/// A node to use in the SCC computation.
#[derive(Clone)]
struct FunctionWithBodyNode<'a> {
    function_with_body_id: FunctionWithBodyId,
    db: &'a dyn LoweringGroup,
}
impl<'a> GraphNode for FunctionWithBodyNode<'a> {
    type NodeId = FunctionWithBodyId;

    fn get_neighbors(&self) -> Vec<Self> {
        self.db
            .function_with_body_direct_function_with_body_callees(self.function_with_body_id)
            .unwrap_or_default()
            .into_iter()
            .map(|function_with_body_id| FunctionWithBodyNode {
                function_with_body_id,
                db: self.db,
            })
            .collect()
    }

    fn get_id(&self) -> Self::NodeId {
        self.function_with_body_id
    }
}

/// Query implementation of [crate::db::LoweringGroup::function_may_panic].
pub fn function_may_panic(db: &dyn LoweringGroup, function: semantic::FunctionId) -> Maybe<bool> {
    match db.lookup_intern_function(function).function.generic_function {
        GenericFunctionId::Free(free_function) => {
            db.function_with_body_may_panic(FunctionWithBodyId::Free(free_function))
        }
        GenericFunctionId::Impl(impl_generic_function) => {
            let impl_function = impl_generic_function.impl_function(db.upcast())?.unwrap();
            db.function_with_body_may_panic(FunctionWithBodyId::Impl(impl_function))
        }
        GenericFunctionId::Extern(extern_function) => {
            Ok(db.extern_function_signature(extern_function)?.panicable)
        }
        GenericFunctionId::Trait(_) => unreachable!(),
    }
}

/// Query implementation of [crate::db::LoweringGroup::function_with_body_may_panic].
pub fn function_with_body_may_panic(
    db: &dyn LoweringGroup,
    function: FunctionWithBodyId,
) -> Maybe<bool> {
    // Find the SCC representative.
    let scc_representative = db.function_scc_representative(function);

    // For each direct callee, find if it may panic.
    for direct_callee in db.function_with_body_direct_callees(function)? {
        // For a function with a body, call this method recursively. To avoid cycles, first
        // check that the callee is not in this function's SCC.
        let direct_callee_representative =
            match db.lookup_intern_function(direct_callee).function.generic_function {
                GenericFunctionId::Free(free_function) => {
                    function_scc_representative(db, FunctionWithBodyId::Free(free_function))
                }
                GenericFunctionId::Impl(impl_generic_function) => {
                    let impl_function = impl_generic_function.impl_function(db.upcast())?.unwrap();
                    function_scc_representative(db, FunctionWithBodyId::Impl(impl_function))
                }
                GenericFunctionId::Extern(extern_function) => {
                    if db.extern_function_signature(extern_function)?.panicable {
                        return Ok(true);
                    }
                    continue;
                }
                GenericFunctionId::Trait(_) => {
                    unreachable!()
                }
            };
        if direct_callee_representative == scc_representative {
            // We already have the implicits of this SCC - do nothing.
            continue;
        }
        if db.function_with_body_may_panic(direct_callee_representative.0)? {
            return Ok(true);
        }
    }
    Ok(false)
}
