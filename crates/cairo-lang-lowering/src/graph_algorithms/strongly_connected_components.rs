use std::cmp::Ordering;

use cairo_lang_defs::source_position::{cmp_elements, cmp_nodes};
use cairo_lang_semantic::source_order::{SourceOrder, cmp_slices};
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
            .min_by(|a, b| a.source_cmp(b, db))
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

/// Lowering functions are ordered by the semantic function they come from, then by kind, so a
/// function precedes the functions generated from it, and those precede its specializations.
impl<'db> SourceOrder<'db> for ConcreteFunctionWithBodyId<'db> {
    fn source_cmp(&self, other: &Self, db: &'db dyn Database) -> Ordering {
        use ConcreteFunctionWithBodyLongId::*;
        let kind = |function: &ConcreteFunctionWithBodyLongId<'db>| match function {
            Semantic(_) => 0,
            Generated(_) => 1,
            Specialized(_) => 2,
        };
        let (a, b) = (self.long(db), other.long(db));
        a.base_semantic_function(db)
            .source_cmp(&b.base_semantic_function(db), db)
            .then_with(|| kind(a).cmp(&kind(b)))
            .then_with(|| match (a, b) {
                (Generated(a), Generated(b)) => a.key.source_cmp(&b.key, db),
                (Specialized(a), Specialized(b)) => {
                    let (a, b) = (a.long(db), b.long(db));
                    a.base.source_cmp(&b.base, db).then_with(|| cmp_slices(db, &a.args, &b.args))
                }
                _ => Ordering::Equal,
            })
    }
}

impl<'db> SourceOrder<'db> for GeneratedFunctionKey<'db> {
    fn source_cmp(&self, other: &Self, db: &'db dyn Database) -> Ordering {
        use GeneratedFunctionKey::*;
        match (self, other) {
            (Loop(a), Loop(b)) => cmp_nodes(db, a.untyped(), b.untyped()),
            (TraitFunc(a_function, a), TraitFunc(b_function, b)) => {
                cmp_nodes(db, a.stable_ptr(), b.stable_ptr())
                    .then_with(|| cmp_elements(db, a_function, b_function))
            }
            (Loop(_), TraitFunc(..)) => Ordering::Less,
            (TraitFunc(..), Loop(_)) => Ordering::Greater,
        }
    }
}

impl<'db> SourceOrder<'db> for SpecializationArg<'db> {
    fn source_cmp(&self, other: &Self, db: &'db dyn Database) -> Ordering {
        use SpecializationArg::*;
        let kind = |arg: &Self| match arg {
            Const { .. } => 0,
            Snapshot(_) => 1,
            Array(..) => 2,
            Struct(_) => 3,
            Enum { .. } => 4,
            NotSpecialized => 5,
        };
        match (self, other) {
            (Const { value: a, boxed: a_boxed }, Const { value: b, boxed: b_boxed }) => {
                a.source_cmp(b, db).then_with(|| a_boxed.cmp(b_boxed))
            }
            (Snapshot(a), Snapshot(b)) => a.source_cmp(b, db),
            (Array(a_ty, a), Array(b_ty, b)) => {
                a_ty.source_cmp(b_ty, db).then_with(|| cmp_slices(db, a, b))
            }
            (Struct(a), Struct(b)) => cmp_slices(db, a, b),
            (Enum { variant: a, payload: a_payload }, Enum { variant: b, payload: b_payload }) => {
                a.source_cmp(b, db).then_with(|| a_payload.source_cmp(b_payload, db))
            }
            (NotSpecialized, NotSpecialized) => Ordering::Equal,
            (a, b) => kind(a).cmp(&kind(b)),
        }
    }
}
