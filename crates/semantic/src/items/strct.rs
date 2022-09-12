use defs::ids::{MemberId, StructId};
use diagnostics_proc_macros::DebugWithDb;

use crate::db::SemanticGroup;
use crate::semantic;

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub struct Struct {
    pub members: Vec<MemberId>,
}

/// Query implementation of [crate::db::SemanticGroup::struct_semantic].
pub fn struct_semantic(_db: &dyn SemanticGroup, _struct_id: StructId) -> semantic::Struct {
    todo!()
}
