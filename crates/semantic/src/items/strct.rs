use defs::diagnostic_utils::StableLocation;
use defs::ids::{LanguageElementId, MemberId, MemberLongId, StructId};
use diagnostics::Diagnostics;
use diagnostics_proc_macros::DebugWithDb;
use smol_str::SmolStr;
use syntax::node::{Terminal, TypedSyntaxNode};
use utils::ordered_hash_map::OrderedHashMap;

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind;
use crate::types::resolve_type;
use crate::{semantic, SemanticDiagnostic};

#[cfg(test)]
#[path = "strct_test.rs"]
mod test;

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub struct StructData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    members: OrderedHashMap<SmolStr, Member>,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub struct Member {
    pub id: MemberId,
    pub ty: semantic::TypeId,
}

/// Query implementation of [crate::db::SemanticGroup::struct_semantic_diagnostics].
pub fn struct_semantic_diagnostics(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_struct_semantic_data(struct_id).map(|data| data.diagnostics).unwrap_or_default()
}
/// Query implementation of [crate::db::SemanticGroup::struct_members].
pub fn struct_members(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Option<OrderedHashMap<SmolStr, Member>> {
    Some(db.priv_struct_semantic_data(struct_id)?.members)
}

/// Query implementation of [crate::db::SemanticGroup::priv_struct_semantic_data].
pub fn priv_struct_semantic_data(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Option<StructData> {
    // TODO(spapini): When asts are rooted on items, don't query module_data directly. Use a
    // selector.
    let mut diagnostics = Diagnostics::default();
    let module_id = struct_id.module(db.upcast());
    let module_data = db.module_data(module_id)?;
    let struct_ast = module_data.structs.get(&struct_id)?;
    let syntax_db = db.upcast();
    let mut members = OrderedHashMap::default();
    for member in struct_ast.members(syntax_db).elements(syntax_db) {
        let id = db.intern_member(MemberLongId(module_id, member.stable_ptr()));
        let ty = resolve_type(
            &mut diagnostics,
            db,
            module_id,
            member.type_clause(syntax_db).ty(syntax_db),
        );
        let member_name = member.name(syntax_db).text(syntax_db);
        if let Some(_other_member) = members.insert(member_name.clone(), Member { id, ty }) {
            diagnostics.add(SemanticDiagnostic {
                stable_location: StableLocation::from_ast(module_id, &member),
                kind: SemanticDiagnosticKind::StructMemberRedefinition { struct_id, member_name },
            })
        }
    }

    Some(StructData { diagnostics, members })
}
