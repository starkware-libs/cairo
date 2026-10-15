use cairo_lang_semantic::stable_order::{StableKey, StableOrd};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_utils::graph_algos::strongly_connected_components::compute_scc;
use salsa::Database;

use super::concrete_function_node::ConcreteFunctionWithBodyNode;
use crate::db::{ConcreteSCCRepresentative, LoweringGroup};
use crate::ids::{
    ConcreteFunctionWithBodyId, ConcreteFunctionWithBodyLongId, GeneratedFunctionKey,
};
use crate::specialization::SpecializationArg;
use crate::{DependencyType, LoweringStage};

/// Query implementation of
/// [crate::db::LoweringGroup::lowered_scc_representative].
#[salsa::tracked(returns(clone))]
pub fn lowered_scc_representative<'db>(
    db: &'db dyn Database,
    function: ConcreteFunctionWithBodyId<'db>,
    dependency_type: DependencyType,
    stage: LoweringStage,
) -> ConcreteSCCRepresentative<'db> {
    ConcreteSCCRepresentative(
        db.lowered_scc(function, dependency_type, stage)
            .into_iter()
            .min_by_key(|function| function.stable_key(db))
            .unwrap_or(function),
    )
}

/// Query implementation of [crate::db::LoweringGroup::lowered_scc].
#[salsa::tracked(returns(clone))]
pub fn lowered_scc<'db>(
    db: &'db dyn Database,
    function_id: ConcreteFunctionWithBodyId<'db>,
    dependency_type: DependencyType,
    stage: LoweringStage,
) -> Vec<ConcreteFunctionWithBodyId<'db>> {
    compute_scc(&ConcreteFunctionWithBodyNode { function_id, db, dependency_type, stage })
}

/// Keyed by the semantic function the lowering function comes from, then by kind.
impl<'db> StableOrd<'db> for ConcreteFunctionWithBodyId<'db> {
    fn write_key(&self, db: &'db dyn Database, key: &mut StableKey<'db>) {
        use ConcreteFunctionWithBodyLongId::*;
        let function = self.long(db);
        key.item(db, &function.base_semantic_function(db));
        match function {
            Semantic(_) => key.variant(line!()),
            Generated(generated) => key.variant(line!()).item(db, &generated.key),
            Specialized(specialized) => {
                let specialized = specialized.long(db);
                key.variant(line!()).item(db, &specialized.base).items(db, &specialized.args)
            }
        };
    }
}

/// Keyed by the syntax the function is generated for.
impl<'db> StableOrd<'db> for GeneratedFunctionKey<'db> {
    fn write_key(&self, db: &'db dyn Database, key: &mut StableKey<'db>) {
        use GeneratedFunctionKey::*;
        match self {
            Loop(stable_ptr) => key.variant(line!()).node(db, stable_ptr.untyped()),
            TraitFunc(function, stable_location) => {
                key.variant(line!()).node(db, stable_location.stable_ptr()).element(db, function)
            }
        };
    }
}

/// Keyed by variant, then by the specialized values.
impl<'db> StableOrd<'db> for SpecializationArg<'db> {
    fn write_key(&self, db: &'db dyn Database, key: &mut StableKey<'db>) {
        use SpecializationArg::*;
        match self {
            Const { value, boxed } => key.variant(line!()).item(db, value).index(*boxed as usize),
            Snapshot(arg) => key.variant(line!()).item(db, arg.as_ref()),
            Array(ty, args) => key.variant(line!()).item(db, ty).items(db, args),
            Struct(args) => key.variant(line!()).items(db, args),
            Enum { variant, payload } => {
                key.variant(line!()).item(db, variant).item(db, payload.as_ref())
            }
            NotSpecialized => key.variant(line!()),
        };
    }
}
