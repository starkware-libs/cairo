use std::sync::Arc;

use cairo_lang_defs::ids::{
    LanguageElementId, LookupItemId, MemberId, MemberLongId, ModuleItemId, StructId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, LookupIntern};
use smol_str::SmolStr;

use super::attribute::SemanticQueryAttrs;
use super::feature_kind::extract_item_feature_config;
use super::generics::{GenericParamsData, semantic_generic_params};
use super::visibility::Visibility;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::inference::InferenceId;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::{GenericSubstitution, SemanticRewriter};
use crate::types::{ConcreteStructId, add_type_based_diagnostics, resolve_type};
use crate::{GenericParam, SemanticDiagnostic, semantic};

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
    let mut diagnostics = SemanticDiagnostics::default();
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
    diagnostics.extend(generic_params_data.diagnostics);

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
    let mut diagnostics = SemanticDiagnostics::default();
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let struct_ast = db.module_struct_by_id(struct_id)?.to_maybe()?;
    // Generic params.
    let inference_id =
        InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(ModuleItemId::Struct(struct_id)));
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    resolver.set_feature_config(&struct_id, &struct_ast, &mut diagnostics);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &struct_ast.generic_params(db.upcast()),
    );
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
    members: Arc<OrderedHashMap<SmolStr, Member>>,
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
    let defs_db = db.upcast();

    let module_file_id = struct_id.module_file_id(defs_db);
    let crate_id = module_file_id.0.owning_crate(defs_db);
    let mut diagnostics = SemanticDiagnostics::default();
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
    diagnostics.extend(generic_params_data.diagnostics);

    // Members.
    let mut members = OrderedHashMap::default();
    for member in struct_ast.members(syntax_db).elements(syntax_db) {
        let feature_restore = resolver
            .data
            .feature_config
            .override_with(extract_item_feature_config(db, crate_id, &member, &mut diagnostics));
        let id = MemberLongId(module_file_id, member.stable_ptr()).intern(db);
        let ty = resolve_type(
            db,
            &mut diagnostics,
            &mut resolver,
            &member.type_clause(syntax_db).ty(syntax_db),
        );
        let visibility =
            Visibility::from_ast(syntax_db, &mut diagnostics, &member.visibility(syntax_db));
        let member_name = member.name(syntax_db).text(syntax_db);
        if let Some(_other_member) =
            members.insert(member_name.clone(), Member { id, ty, visibility })
        {
            diagnostics.report(&member, StructMemberRedefinition { struct_id, member_name });
        }
        resolver.data.feature_config.restore(feature_restore);
    }

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, struct_ast.stable_ptr().untyped());

    for (_, member) in members.iter_mut() {
        member.ty = inference.rewrite(member.ty).no_err();
    }

    let resolver_data = Arc::new(resolver.data);
    Ok(StructDefinitionData {
        diagnostics: diagnostics.build(),
        members: members.into(),
        resolver_data,
    })
}

/// Query implementation of [crate::db::SemanticGroup::struct_definition_diagnostics].
pub fn struct_definition_diagnostics(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Diagnostics<SemanticDiagnostic> {
    let Ok(data) = db.priv_struct_definition_data(struct_id) else {
        return Default::default();
    };

    let crate_id = data.resolver_data.module_file_id.0.owning_crate(db.upcast());

    // If the struct is a phantom type, no need to check if its members are fully valid types, as
    // they won't be used.
    if db
        .declared_phantom_type_attributes(crate_id)
        .iter()
        .any(|attr| struct_id.has_attr(db, attr).unwrap_or_default())
    {
        return data.diagnostics;
    }
    let mut diagnostics = SemanticDiagnostics::from(data.diagnostics);
    for (_, member) in data.members.iter() {
        let stable_ptr = member.id.stable_ptr(db.upcast());
        add_type_based_diagnostics(db, &mut diagnostics, member.ty, stable_ptr);
        if member.ty.is_phantom(db) {
            diagnostics.report(stable_ptr, NonPhantomTypeContainingPhantomType);
        }
    }
    diagnostics.build()
}

/// Query implementation of [crate::db::SemanticGroup::struct_members].
pub fn struct_members(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Maybe<Arc<OrderedHashMap<SmolStr, Member>>> {
    Ok(db.priv_struct_definition_data(struct_id)?.members)
}

/// Query implementation of [crate::db::SemanticGroup::struct_definition_resolver_data].
pub fn struct_definition_resolver_data(
    db: &dyn SemanticGroup,
    struct_id: StructId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_struct_definition_data(struct_id)?.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::concrete_struct_members].
pub fn concrete_struct_members(
    db: &dyn SemanticGroup,
    concrete_struct_id: ConcreteStructId,
) -> Maybe<Arc<OrderedHashMap<SmolStr, semantic::Member>>> {
    // TODO(spapini): Uphold the invariant that constructed ConcreteEnumId instances
    //   always have the correct number of generic arguments.
    let generic_params = db.struct_generic_params(concrete_struct_id.struct_id(db))?;
    let generic_args = concrete_struct_id.lookup_intern(db).generic_args;
    let substitution = GenericSubstitution::new(&generic_params, &generic_args);

    let generic_members = db.struct_members(concrete_struct_id.struct_id(db))?;
    Ok(Arc::new(
        generic_members
            .iter()
            .map(|(name, member)| {
                let ty = substitution.substitute(db, member.ty)?;
                Ok((name.clone(), semantic::Member { ty, ..member.clone() }))
            })
            .collect::<Maybe<_>>()?,
    ))
}
