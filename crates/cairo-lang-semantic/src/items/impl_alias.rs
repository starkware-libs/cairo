use std::sync::Arc;

use cairo_lang_defs::ids::{
    ImplAliasId, ImplDefId, LanguageElementId, LookupItemId, ModuleFileId, ModuleItemId,
};
use cairo_lang_diagnostics::{skip_diagnostic, Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::{ast, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::try_extract_matches;

use super::generics::{semantic_generic_params, GenericParamsData};
use super::imp::ImplId;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::InferenceId;
use crate::resolve::{ResolvedConcreteItem, ResolvedGenericItem, Resolver, ResolverData};
use crate::substitution::SemanticRewriter;
use crate::{GenericParam, SemanticDiagnostic};

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplAliasData {
    pub diagnostics: Diagnostics<SemanticDiagnostic>,
    pub resolved_impl: Maybe<ImplId>,
    generic_params: Vec<GenericParam>,
    attributes: Vec<Attribute>,
    pub resolver_data: Arc<ResolverData>,
}

/// Query implementation of [crate::db::SemanticGroup::priv_impl_alias_semantic_data].
pub fn priv_impl_alias_semantic_data(
    db: &(dyn SemanticGroup),
    impl_alias_id: ImplAliasId,
    in_cycle: bool,
) -> Maybe<ImplAliasData> {
    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::ImplAlias(impl_alias_id));
    let impl_alias_ast = db.module_impl_alias_by_id(impl_alias_id)?.to_maybe()?;

    let generic_params_data = db.impl_alias_generic_params_data(impl_alias_id)?;

    if in_cycle {
        impl_alias_semantic_data_cycle_helper(
            db,
            &impl_alias_ast,
            lookup_item_id,
            generic_params_data,
        )
    } else {
        impl_alias_semantic_data_helper(db, &impl_alias_ast, lookup_item_id, generic_params_data)
    }
}

/// An helper function to compute the semantic data of an impl-alias item.
pub fn impl_alias_semantic_data_helper(
    db: &(dyn SemanticGroup),
    impl_alias_ast: &ast::ItemImplAlias,
    lookup_item_id: LookupItemId,
    generic_params_data: GenericParamsData,
) -> Maybe<ImplAliasData> {
    let mut diagnostics = SemanticDiagnostics::default();
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let syntax_db = db.upcast();
    let inference_id = InferenceId::LookupItemDeclaration(lookup_item_id);
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics);

    let item = resolver.resolve_concrete_path(
        &mut diagnostics,
        &impl_alias_ast.impl_path(syntax_db),
        NotFoundItemType::Impl,
    );
    let resolved_impl = item.and_then(|item| {
        try_extract_matches!(item, ResolvedConcreteItem::Impl)
            .ok_or_else(|| diagnostics.report(&impl_alias_ast.impl_path(syntax_db), UnknownImpl))
    });

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, impl_alias_ast.stable_ptr().untyped());

    let resolved_impl = inference.rewrite(resolved_impl).no_err();
    let generic_params = inference.rewrite(generic_params_data.generic_params.clone()).no_err();

    let attributes = impl_alias_ast.attributes(syntax_db).structurize(syntax_db);
    let resolver_data = Arc::new(resolver.data);
    Ok(ImplAliasData {
        diagnostics: diagnostics.build(),
        resolved_impl,
        generic_params,
        attributes,
        resolver_data,
    })
}

/// Cycle handling for [crate::db::SemanticGroup::priv_impl_alias_semantic_data].
pub fn priv_impl_alias_semantic_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_alias_id: &ImplAliasId,
    _in_cycle: &bool,
) -> Maybe<ImplAliasData> {
    priv_impl_alias_semantic_data(db, *impl_alias_id, true)
}

/// An helper function to compute the semantic data of an impl-alias item when a cycle is detected.
pub fn impl_alias_semantic_data_cycle_helper(
    db: &(dyn SemanticGroup),
    impl_alias_ast: &ast::ItemImplAlias,
    lookup_item_id: LookupItemId,
    generic_params_data: GenericParamsData,
) -> Maybe<ImplAliasData> {
    let mut diagnostics = SemanticDiagnostics::default();
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let syntax_db = db.upcast();
    let err = Err(diagnostics.report(&impl_alias_ast.name(syntax_db), ImplAliasCycle));
    let generic_params = generic_params_data.generic_params.clone();
    diagnostics.extend(generic_params_data.diagnostics);
    let inference_id = InferenceId::LookupItemDeclaration(lookup_item_id);
    let attributes = impl_alias_ast.attributes(syntax_db).structurize(syntax_db);
    Ok(ImplAliasData {
        diagnostics: diagnostics.build(),
        resolved_impl: err,
        generic_params,
        attributes,
        resolver_data: (*generic_params_data.resolver_data)
            .clone_with_inference_id(db, inference_id)
            .into(),
    })
}

/// Query implementation of [crate::db::SemanticGroup::impl_alias_semantic_diagnostics].
pub fn impl_alias_semantic_diagnostics(
    db: &dyn SemanticGroup,
    impl_alias_id: ImplAliasId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_impl_alias_semantic_data(impl_alias_id, false)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::impl_alias_resolved_impl].
pub fn impl_alias_resolved_impl(
    db: &dyn SemanticGroup,
    impl_alias_id: ImplAliasId,
) -> Maybe<ImplId> {
    db.priv_impl_alias_semantic_data(impl_alias_id, false)?.resolved_impl
}

/// Trivial cycle handling for [crate::db::SemanticGroup::impl_alias_resolved_impl].
pub fn impl_alias_resolved_impl_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_alias_id: &ImplAliasId,
) -> Maybe<ImplId> {
    // Forwarding (not as a query) cycle handling to `priv_impl_alias_semantic_data` cycle handler.
    db.priv_impl_alias_semantic_data(*impl_alias_id, true)?.resolved_impl
}

/// Query implementation of [crate::db::SemanticGroup::impl_alias_generic_params].
pub fn impl_alias_generic_params(
    db: &dyn SemanticGroup,
    impl_alias_id: ImplAliasId,
) -> Maybe<Vec<GenericParam>> {
    Ok(db.impl_alias_generic_params_data(impl_alias_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::impl_alias_generic_params_data].
pub fn impl_alias_generic_params_data(
    db: &dyn SemanticGroup,
    impl_alias_id: ImplAliasId,
) -> Maybe<GenericParamsData> {
    let module_file_id = impl_alias_id.module_file_id(db.upcast());
    let impl_alias_ast = db.module_impl_alias_by_id(impl_alias_id)?.to_maybe()?;
    impl_alias_generic_params_data_helper(
        db,
        module_file_id,
        &impl_alias_ast,
        LookupItemId::ModuleItem(ModuleItemId::ImplAlias(impl_alias_id)),
        None,
    )
}

/// Computes data about the generic parameters of an impl-alias item.
pub fn impl_alias_generic_params_data_helper(
    db: &dyn SemanticGroup,
    module_file_id: ModuleFileId,
    impl_alias_ast: &ast::ItemImplAlias,
    lookup_item_id: LookupItemId,
    parent_resolver_data: Option<Arc<ResolverData>>,
) -> Maybe<GenericParamsData> {
    let mut diagnostics = SemanticDiagnostics::default();
    let inference_id = InferenceId::LookupItemGenerics(lookup_item_id);

    let mut resolver = match parent_resolver_data {
        Some(parent_resolver_data) => {
            Resolver::with_data(db, parent_resolver_data.clone_with_inference_id(db, inference_id))
        }
        None => Resolver::new(db, module_file_id, inference_id),
    };
    resolver.set_feature_config(&lookup_item_id, impl_alias_ast, &mut diagnostics);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &impl_alias_ast.generic_params(db.upcast()),
    );

    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, impl_alias_ast.stable_ptr().untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { diagnostics: diagnostics.build(), generic_params, resolver_data })
}

/// Query implementation of [crate::db::SemanticGroup::impl_alias_resolver_data].
pub fn impl_alias_resolver_data(
    db: &dyn SemanticGroup,
    impl_alias_id: ImplAliasId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_impl_alias_semantic_data(impl_alias_id, false)?.resolver_data)
}

/// Trivial cycle handling for [crate::db::SemanticGroup::impl_alias_resolver_data].
pub fn impl_alias_resolver_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_alias_id: &ImplAliasId,
) -> Maybe<Arc<ResolverData>> {
    // Forwarding (not as a query) cycle handling to `priv_impl_alias_semantic_data` cycle handler.
    impl_alias_resolver_data(db, *impl_alias_id)
}

/// Query implementation of [crate::db::SemanticGroup::impl_alias_attributes].
pub fn impl_alias_attributes(
    db: &dyn SemanticGroup,
    impl_alias_id: ImplAliasId,
) -> Maybe<Vec<Attribute>> {
    Ok(db.priv_impl_alias_semantic_data(impl_alias_id, false)?.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::impl_alias_impl_def].
pub fn impl_alias_impl_def(db: &dyn SemanticGroup, impl_alias_id: ImplAliasId) -> Maybe<ImplDefId> {
    let module_file_id = impl_alias_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::default();
    let impl_alias_ast = db.module_impl_alias_by_id(impl_alias_id)?.to_maybe()?;
    let inference_id = InferenceId::ImplAliasImplDef(impl_alias_id);

    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    resolver.set_feature_config(&impl_alias_id, &impl_alias_ast, &mut diagnostics);

    let impl_path_syntax = impl_alias_ast.impl_path(db.upcast());

    match resolver.resolve_generic_path_with_args(
        &mut diagnostics,
        &impl_path_syntax,
        NotFoundItemType::Impl,
    ) {
        Ok(ResolvedGenericItem::Impl(imp)) => Ok(imp),
        Ok(ResolvedGenericItem::GenericImplAlias(impl_alias)) => db.impl_alias_impl_def(impl_alias),
        // Skipping diagnostics since we will get these through when resolving in the
        // `priv_impl_alias_semantic_data` query.
        _ => Err(skip_diagnostic()),
    }
}

/// Cycle handling for [crate::db::SemanticGroup::impl_alias_impl_def].
pub fn impl_alias_impl_def_cycle(
    _db: &dyn SemanticGroup,
    _cycle: &[String],
    _impl_alias_id: &ImplAliasId,
) -> Maybe<ImplDefId> {
    // Skipping diagnostics since we will get these through when resolving in the
    // `priv_impl_alias_semantic_data` query.
    Err(skip_diagnostic())
}
