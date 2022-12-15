use std::sync::Arc;

use db_utils::Upcast;
use defs::ids::{GenericParamId, LanguageElementId, MemberId, MemberLongId, StructId};
use diagnostics::{Diagnostics, Maybe, ToMaybe};
use diagnostics_proc_macros::DebugWithDb;
use smol_str::SmolStr;
use syntax::node::{Terminal, TypedSyntaxNode};
use utils::ordered_hash_map::OrderedHashMap;

use super::attribute::{ast_attributes_to_semantic, Attribute};
use super::generics::semantic_generic_params;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::resolve_path::{ResolvedLookback, Resolver};
use crate::types::{resolve_type, substitute_generics, ConcreteStructId};
use crate::{semantic, SemanticDiagnostic};

#[cfg(test)]
#[path = "strct_test.rs"]
mod test;

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct StructData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    generic_params: Vec<GenericParamId>,
    members: OrderedHashMap<SmolStr, Member>,
    attributes: Vec<Attribute>,
    resolved_lookback: Arc<ResolvedLookback>,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
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

/// Query implementation of [crate::db::SemanticGroup::struct_generic_params].
pub fn struct_generic_params(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Maybe<Vec<GenericParamId>> {
    Ok(db.priv_struct_semantic_data(struct_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::struct_members].
pub fn struct_members(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Maybe<OrderedHashMap<SmolStr, Member>> {
    Ok(db.priv_struct_semantic_data(struct_id)?.members)
}

/// Query implementation of [crate::db::SemanticGroup::struct_attributes].
pub fn struct_attributes(db: &dyn SemanticGroup, struct_id: StructId) -> Maybe<Vec<Attribute>> {
    Ok(db.priv_struct_semantic_data(struct_id)?.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::struct_resolved_lookback].
pub fn struct_resolved_lookback(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Maybe<Arc<ResolvedLookback>> {
    Ok(db.priv_struct_semantic_data(struct_id)?.resolved_lookback)
}

/// Query implementation of [crate::db::SemanticGroup::priv_struct_semantic_data].
pub fn priv_struct_semantic_data(db: &dyn SemanticGroup, struct_id: StructId) -> Maybe<StructData> {
    // TODO(spapini): When asts are rooted on items, don't query module_data directly. Use a
    // selector.
    let module_file_id = struct_id.module_file(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    // TODO(spapini): Add generic args when they are supported on structs.
    let module_data = db.module_data(module_file_id.0)?;
    let struct_ast = module_data.structs.get(&struct_id).to_maybe()?;
    let syntax_db = db.upcast();

    // Generic params.
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        module_file_id,
        &struct_ast.generic_params(db.upcast()),
    );
    let mut resolver = Resolver::new(db, module_file_id, &generic_params);

    // Members.
    let mut members = OrderedHashMap::default();
    for member in struct_ast.members(syntax_db).elements(syntax_db) {
        let id = db.intern_member(MemberLongId(module_file_id, member.stable_ptr()));
        let ty = resolve_type(
            db,
            &mut diagnostics,
            &mut resolver,
            &member.type_clause(syntax_db).ty(syntax_db),
        );
        let member_name = member.name(syntax_db).text(syntax_db);
        if let Some(_other_member) = members.insert(member_name.clone(), Member { id, ty }) {
            diagnostics.report(&member, StructMemberRedefinition { struct_id, member_name });
        }
    }

    let attributes = ast_attributes_to_semantic(syntax_db, struct_ast.attributes(syntax_db));
    let resolved_lookback = Arc::new(resolver.lookback);

    Ok(StructData {
        diagnostics: diagnostics.build(),
        generic_params,
        members,
        attributes,
        resolved_lookback,
    })
}

pub trait SemanticStructEx<'a>: Upcast<dyn SemanticGroup + 'a> {
    fn concrete_struct_members(
        &self,
        concrete_struct_id: ConcreteStructId,
    ) -> Maybe<OrderedHashMap<SmolStr, semantic::Member>> {
        // TODO(spapini): Uphold the invariant that constructed ConcreteEnumId instances
        //   always have the correct number of generic arguemnts.
        let db = self.upcast();
        let generic_params = db.struct_generic_params(concrete_struct_id.struct_id(db))?;
        let generic_args = db.lookup_intern_concrete_struct(concrete_struct_id).generic_args;
        let substitution = &generic_params.into_iter().zip(generic_args.into_iter()).collect();

        let generic_members =
            self.upcast().struct_members(concrete_struct_id.struct_id(self.upcast()))?;
        Ok(generic_members
            .into_iter()
            .map(|(name, member)| {
                let ty = substitute_generics(db, substitution, member.ty);
                let member = semantic::Member { ty, ..member };
                (name, member)
            })
            .collect())
    }
}

impl<'a, T: Upcast<dyn SemanticGroup + 'a> + ?Sized> SemanticStructEx<'a> for T {}
