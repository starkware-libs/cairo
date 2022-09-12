use defs::ids::{MemberId, StructId};
use diagnostics::Diagnostics;
use diagnostics_proc_macros::DebugWithDb;

use crate::db::SemanticGroup;
use crate::SemanticDiagnostic;

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub struct StructData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    members: Vec<MemberId>,
}
/// Query implementation of [crate::db::SemanticGroup::struct_semantic_diagnostics].
pub fn struct_semantic_diagnostics(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_struct_semantic_data(struct_id).diagnostics
}
/// Query implementation of [crate::db::SemanticGroup::struct_members].
pub fn struct_members(db: &dyn SemanticGroup, struct_id: StructId) -> Vec<MemberId> {
    db.priv_struct_semantic_data(struct_id).members
}

/// Query implementation of [crate::db::SemanticGroup::priv_struct_semantic_data].
pub fn priv_struct_semantic_data(_db: &dyn SemanticGroup, _struct_id: StructId) -> StructData {
    todo!()
}
