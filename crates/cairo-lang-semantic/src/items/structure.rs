use std::sync::Arc;

use cairo_lang_defs::ids::{
    LanguageElementId, LookupItemId, MemberId, MemberLongId, ModuleItemId, StructId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::Upcast;
use smol_str::SmolStr;

use super::generics::{semantic_generic_params, GenericParamsData};
use super::visibility::Visibility;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::InferenceId;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::{GenericSubstitution, SemanticRewriter, SubstitutionRewriter};
use crate::types::{resolve_type, ConcreteStructId};
use crate::{semantic, GenericParam, SemanticDiagnostic};

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
    resolver_data: Arc<ResolverData>,
}

/// Query implementation of [crate::db::SemanticGroup::priv_struct_declaration_data].
pub fn priv_struct_declaration_data(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Maybe<StructDeclarationData> {
    let module_file_id = struct_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let struct_ast = db.module_struct_by_id(struct_id)?.to_maybe()?;
    let syntax_db = db.upcast();

    // Generic params.
    let generic_params_data = db.struct_generic_params_data(struct_id)?;
    let generic_params = generic_params_data.generic_params;
    let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(
        ModuleItemId::Struct(struct_id),
    ));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.diagnostics.extend(generic_params_data.diagnostics);

    let attributes = struct_ast.attributes(syntax_db).structurize(syntax_db);

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, struct_ast.stable_ptr().untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(StructDeclarationData {
        diagnostics: diagnostics.build(),
        generic_params,
        attributes,
        resolver_data,
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
) -> Maybe<Vec<GenericParam>> {
    db.struct_generic_params_data(struct_id).map(|data| data.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::struct_generic_params_data].
pub fn struct_generic_params_data(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Maybe<GenericParamsData> {
    let module_file_id = struct_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let struct_ast = db.module_struct_by_id(struct_id)?.to_maybe()?;
    // Generic params.
    let inference_id =
        InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(ModuleItemId::Struct(struct_id)));
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &struct_ast.generic_params(db.upcast()),
    )?;
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, struct_ast.stable_ptr().untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { generic_params, diagnostics: diagnostics.build(), resolver_data })
}

/// Query implementation of [crate::db::SemanticGroup::struct_attributes].
pub fn struct_attributes(db: &dyn SemanticGroup, struct_id: StructId) -> Maybe<Vec<Attribute>> {
    Ok(db.priv_struct_declaration_data(struct_id)?.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::struct_declaration_resolver_data].
pub fn struct_declaration_resolver_data(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_struct_declaration_data(struct_id)?.resolver_data)
}

// Definition.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct StructDefinitionData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    members: OrderedHashMap<SmolStr, Member>,
    resolver_data: Arc<ResolverData>,
}
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct Member {
    pub id: MemberId,
    pub ty: semantic::TypeId,
    #[dont_rewrite]
    pub visibility: Visibility,
}

/// Query implementation of [crate::db::SemanticGroup::priv_struct_definition_data].
pub fn priv_struct_definition_data(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Maybe<StructDefinitionData> {
    let module_file_id = struct_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let struct_ast = db.module_struct_by_id(struct_id)?.to_maybe()?;
    let syntax_db = db.upcast();

    // Generic params.
    let generic_params_data = db.struct_generic_params_data(struct_id)?;
    let inference_id = InferenceId::LookupItemDefinition(LookupItemId::ModuleItem(
        ModuleItemId::Struct(struct_id),
    ));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.diagnostics.extend(generic_params_data.diagnostics);

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
        let visibility = Visibility::from_ast(
            syntax_db,
            &mut diagnostics.diagnostics,
            &member.visibility(syntax_db),
        );
        let member_name = member.name(syntax_db).text(syntax_db);
        if let Some(_other_member) =
            members.insert(member_name.clone(), Member { id, ty, visibility })
        {
            diagnostics.report(&member, StructMemberRedefinition { struct_id, member_name });
        }
    }

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, struct_ast.stable_ptr().untyped());

    for (_, member) in members.iter_mut() {
        member.ty = inference.rewrite(member.ty).no_err();
    }

    let resolver_data = Arc::new(resolver.data);
    Ok(StructDefinitionData { diagnostics: diagnostics.build(), members, resolver_data })
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

/// Query implementation of [crate::db::SemanticGroup::struct_definition_resolver_data].
pub fn struct_definition_resolver_data(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_struct_declaration_data(struct_id)?.resolver_data)
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
