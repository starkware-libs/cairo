use std::sync::Arc;

use cairo_lang_defs::ids::{LanguageElementId, MemberId, MemberLongId, StructId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::Upcast;
use smol_str::SmolStr;

use super::generics::semantic_generic_params;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::resolve::{ResolvedItems, Resolver};
use crate::substitution::{GenericSubstitution, SemanticRewriter, SubstitutionRewriter};
use crate::types::{resolve_type, ConcreteStructId};
use crate::{semantic, SemanticDiagnostic};

#[cfg(test)]
#[path = "structure_test.rs"]
mod test;

// TODO(spapini): Check for bad recursive types - those that will fail in Sierra generation.

// Declaration.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct StructDeclarationData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    generic_params: Vec<semantic::GenericParam>,
    attributes: Vec<Attribute>,
    resolved_lookback: Arc<ResolvedItems>,
}

/// Query implementation of [crate::db::SemanticGroup::priv_struct_declaration_data].
pub fn priv_struct_declaration_data(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Maybe<StructDeclarationData> {
    let module_file_id = struct_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let module_structs = db.module_structs(module_file_id.0)?;
    let struct_ast = module_structs.get(&struct_id).to_maybe()?;
    let syntax_db = db.upcast();

    // Generic params.
    let mut resolver = Resolver::new(db, module_file_id);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &struct_ast.generic_params(db.upcast()),
    )?;

    let attributes = struct_ast.attributes(syntax_db).structurize(syntax_db);
    let resolved_lookback = Arc::new(resolver.resolved_items);

    // Check fully resolved.
    if let Some((stable_ptr, inference_err)) = resolver.inference.finalize() {
        inference_err.report(&mut diagnostics, stable_ptr);
    }
    let generic_params = resolver
        .inference
        .rewrite(generic_params)
        .map_err(|err| err.report(&mut diagnostics, struct_ast.stable_ptr().untyped()))?;

    Ok(StructDeclarationData {
        diagnostics: diagnostics.build(),
        generic_params,
        attributes,
        resolved_lookback,
    })
}

/// Query implementation of [crate::db::SemanticGroup::struct_declaration_diagnostics].
pub fn struct_declaration_diagnostics(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_struct_declaration_data(struct_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::struct_generic_params].
pub fn struct_generic_params(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Maybe<Vec<semantic::GenericParam>> {
    Ok(db.priv_struct_declaration_data(struct_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::struct_attributes].
pub fn struct_attributes(db: &dyn SemanticGroup, struct_id: StructId) -> Maybe<Vec<Attribute>> {
    Ok(db.priv_struct_declaration_data(struct_id)?.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::struct_declaration_resolved_lookback].
pub fn struct_declaration_resolved_lookback(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Maybe<Arc<ResolvedItems>> {
    Ok(db.priv_struct_declaration_data(struct_id)?.resolved_lookback)
}

// Definition.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct StructDefinitionData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    members: OrderedHashMap<SmolStr, Member>,
    resolved_lookback: Arc<ResolvedItems>,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct Member {
    pub id: MemberId,
    pub ty: semantic::TypeId,
}

/// Query implementation of [crate::db::SemanticGroup::priv_struct_definition_data].
pub fn priv_struct_definition_data(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Maybe<StructDefinitionData> {
    let module_file_id = struct_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let module_structs = db.module_structs(module_file_id.0)?;
    let struct_ast = module_structs.get(&struct_id).to_maybe()?;
    let syntax_db = db.upcast();

    // Generic params.
    let mut resolver = Resolver::new(db, module_file_id);
    let generic_params = db.struct_generic_params(struct_id)?;
    for generic_param in generic_params {
        resolver.add_generic_param(generic_param);
    }

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

    let resolved_lookback = Arc::new(resolver.resolved_items);

    // Check fully resolved.
    if let Some((stable_ptr, inference_err)) = resolver.inference.finalize() {
        inference_err.report(&mut diagnostics, stable_ptr);
    }
    for (_, member) in members.iter_mut() {
        member.ty = resolver
            .inference
            .rewrite(member.ty)
            .map_err(|err| err.report(&mut diagnostics, struct_ast.stable_ptr().untyped()))?;
    }

    Ok(StructDefinitionData { diagnostics: diagnostics.build(), members, resolved_lookback })
}

/// Query implementation of [crate::db::SemanticGroup::struct_definition_diagnostics].
pub fn struct_definition_diagnostics(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_struct_definition_data(struct_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::struct_members].
pub fn struct_members(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Maybe<OrderedHashMap<SmolStr, Member>> {
    Ok(db.priv_struct_definition_data(struct_id)?.members)
}

/// Query implementation of [crate::db::SemanticGroup::struct_definition_resolved_lookback].
pub fn struct_definition_resolved_lookback(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Maybe<Arc<ResolvedItems>> {
    Ok(db.priv_struct_declaration_data(struct_id)?.resolved_lookback)
}

pub trait SemanticStructEx<'a>: Upcast<dyn SemanticGroup + 'a> {
    fn concrete_struct_members(
        &self,
        concrete_struct_id: ConcreteStructId,
    ) -> Maybe<OrderedHashMap<SmolStr, semantic::Member>> {
        // TODO(spapini): Uphold the invariant that constructed ConcreteEnumId instances
        //   always have the correct number of generic arguments.
        let db = self.upcast();
        let generic_params = db.struct_generic_params(concrete_struct_id.struct_id(db))?;
        let generic_args = db.lookup_intern_concrete_struct(concrete_struct_id).generic_args;
        let substitution = GenericSubstitution::new(&generic_params, &generic_args);

        let generic_members =
            self.upcast().struct_members(concrete_struct_id.struct_id(self.upcast()))?;
        generic_members
            .into_iter()
            .map(|(name, member)| {
                let ty =
                    SubstitutionRewriter { db, substitution: &substitution }.rewrite(member.ty)?;
                let member = semantic::Member { ty, ..member };
                Ok((name, member))
            })
            .collect::<Maybe<_>>()
    }
}

impl<'a, T: Upcast<dyn SemanticGroup + 'a> + ?Sized> SemanticStructEx<'a> for T {}
