use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    ImplAliasId, ImplDefId, LanguageElementId, LookupItemId, ModuleFileId, ModuleItemId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, skip_diagnostic};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::try_extract_matches;

use super::generics::{GenericParamsData, semantic_generic_params};
use super::imp::ImplId;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::inference::InferenceId;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::resolve::{
    ResolutionContext, ResolvedConcreteItem, ResolvedGenericItem, Resolver, ResolverData,
};
use crate::substitution::SemanticRewriter;
use crate::{GenericParam, SemanticDiagnostic};

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn SemanticGroup)]
pub struct ImplAliasData<'db> {
    pub diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    pub resolved_impl: Maybe<ImplId<'db>>,
    generic_params: Vec<GenericParam<'db>>,
    attributes: Vec<Attribute<'db>>,
    pub resolver_data: Arc<ResolverData<'db>>,
}

/// Implementation of [crate::db::SemanticGroup::priv_impl_alias_semantic_data].
pub fn priv_impl_alias_semantic_data<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
    in_cycle: bool,
) -> Maybe<ImplAliasData<'db>> {
    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::ImplAlias(impl_alias_id));
    let impl_alias_ast = db.module_impl_alias_by_id(impl_alias_id)?;

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

/// Query implementation of [crate::db::SemanticGroup::priv_impl_alias_semantic_data].
#[salsa::tracked(cycle_result=priv_impl_alias_semantic_data_cycle)]
pub fn priv_impl_alias_semantic_data_tracked<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
    in_cycle: bool,
) -> Maybe<ImplAliasData<'db>> {
    priv_impl_alias_semantic_data(db, impl_alias_id, in_cycle)
}

/// A helper function to compute the semantic data of an impl-alias item.
pub fn impl_alias_semantic_data_helper<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_ast: &ast::ItemImplAlias<'db>,
    lookup_item_id: LookupItemId<'db>,
    generic_params_data: GenericParamsData<'db>,
) -> Maybe<ImplAliasData<'db>> {
    let mut diagnostics = SemanticDiagnostics::default();
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let inference_id = InferenceId::LookupItemDeclaration(lookup_item_id);
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics);

    let item = resolver.resolve_concrete_path(
        &mut diagnostics,
        &impl_alias_ast.impl_path(db),
        NotFoundItemType::Impl,
    );
    let resolved_impl = item.and_then(|item| {
        try_extract_matches!(item, ResolvedConcreteItem::Impl).ok_or_else(|| {
            diagnostics.report(impl_alias_ast.impl_path(db).stable_ptr(db), UnknownImpl)
        })
    });

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, impl_alias_ast.stable_ptr(db).untyped());

    let resolved_impl = inference.rewrite(resolved_impl).no_err();
    let generic_params = inference.rewrite(generic_params_data.generic_params).no_err();

    let attributes = impl_alias_ast.attributes(db).structurize(db);
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
pub fn priv_impl_alias_semantic_data_cycle<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
    _in_cycle: bool,
) -> Maybe<ImplAliasData<'db>> {
    priv_impl_alias_semantic_data(db, impl_alias_id, true)
}

/// A helper function to compute the semantic data of an impl-alias item when a cycle is detected.
pub fn impl_alias_semantic_data_cycle_helper<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_ast: &ast::ItemImplAlias<'db>,
    lookup_item_id: LookupItemId<'db>,
    generic_params_data: GenericParamsData<'db>,
) -> Maybe<ImplAliasData<'db>> {
    let mut diagnostics = SemanticDiagnostics::default();
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let err = Err(diagnostics.report(impl_alias_ast.name(db).stable_ptr(db), ImplAliasCycle));
    let generic_params = generic_params_data.generic_params.clone();
    diagnostics.extend(generic_params_data.diagnostics);
    let inference_id = InferenceId::LookupItemDeclaration(lookup_item_id);
    let attributes = impl_alias_ast.attributes(db).structurize(db);
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

/// Implementation of [crate::db::SemanticGroup::impl_alias_semantic_diagnostics].
pub fn impl_alias_semantic_diagnostics<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    db.priv_impl_alias_semantic_data(impl_alias_id, false)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::impl_alias_semantic_diagnostics].
#[salsa::tracked]
pub fn impl_alias_semantic_diagnostics_tracked<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    impl_alias_semantic_diagnostics(db, impl_alias_id)
}

/// Implementation of [crate::db::SemanticGroup::impl_alias_resolved_impl].
pub fn impl_alias_resolved_impl<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
) -> Maybe<ImplId<'db>> {
    db.priv_impl_alias_semantic_data(impl_alias_id, false)?.resolved_impl
}

/// Query implementation of [crate::db::SemanticGroup::impl_alias_resolved_impl].
#[salsa::tracked(cycle_result=impl_alias_resolved_impl_cycle)]
pub fn impl_alias_resolved_impl_tracked<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
) -> Maybe<ImplId<'db>> {
    impl_alias_resolved_impl(db, impl_alias_id)
}

/// Trivial cycle handling for [crate::db::SemanticGroup::impl_alias_resolved_impl].
pub fn impl_alias_resolved_impl_cycle<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
) -> Maybe<ImplId<'db>> {
    // Forwarding (not as a query) cycle handling to `priv_impl_alias_semantic_data` cycle handler.
    db.priv_impl_alias_semantic_data(impl_alias_id, true)?.resolved_impl
}

/// Implementation of [crate::db::SemanticGroup::impl_alias_generic_params].
pub fn impl_alias_generic_params<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
) -> Maybe<Vec<GenericParam<'db>>> {
    Ok(db.impl_alias_generic_params_data(impl_alias_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::impl_alias_generic_params].
#[salsa::tracked]
pub fn impl_alias_generic_params_tracked<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
) -> Maybe<Vec<GenericParam<'db>>> {
    impl_alias_generic_params(db, impl_alias_id)
}

/// Implementation of [crate::db::SemanticGroup::impl_alias_generic_params_data].
pub fn impl_alias_generic_params_data<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    let module_file_id = impl_alias_id.module_file_id(db);
    let impl_alias_ast = db.module_impl_alias_by_id(impl_alias_id)?;
    impl_alias_generic_params_data_helper(
        db,
        module_file_id,
        &impl_alias_ast,
        LookupItemId::ModuleItem(ModuleItemId::ImplAlias(impl_alias_id)),
        None,
    )
}

/// Query implementation of [crate::db::SemanticGroup::impl_alias_generic_params_data].
#[salsa::tracked]
pub fn impl_alias_generic_params_data_tracked<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    impl_alias_generic_params_data(db, impl_alias_id)
}

/// Computes data about the generic parameters of an impl-alias item.
pub fn impl_alias_generic_params_data_helper<'db>(
    db: &'db dyn SemanticGroup,
    module_file_id: ModuleFileId<'db>,
    impl_alias_ast: &ast::ItemImplAlias<'db>,
    lookup_item_id: LookupItemId<'db>,
    parent_resolver_data: Option<Arc<ResolverData<'db>>>,
) -> Maybe<GenericParamsData<'db>> {
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
        &impl_alias_ast.generic_params(db),
    );

    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, impl_alias_ast.stable_ptr(db).untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { diagnostics: diagnostics.build(), generic_params, resolver_data })
}

/// Implementation of [crate::db::SemanticGroup::impl_alias_resolver_data].
pub fn impl_alias_resolver_data<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    Ok(db.priv_impl_alias_semantic_data(impl_alias_id, false)?.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::impl_alias_resolver_data].
#[salsa::tracked(cycle_result=impl_alias_resolver_data_cycle)]
pub fn impl_alias_resolver_data_tracked<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    impl_alias_resolver_data(db, impl_alias_id)
}

/// Trivial cycle handling for [crate::db::SemanticGroup::impl_alias_resolver_data].
pub fn impl_alias_resolver_data_cycle<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    // Forwarding (not as a query) cycle handling to `priv_impl_alias_semantic_data` cycle handler.
    impl_alias_resolver_data(db, impl_alias_id)
}

/// Implementation of [crate::db::SemanticGroup::impl_alias_attributes].
pub fn impl_alias_attributes<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
) -> Maybe<Vec<Attribute<'db>>> {
    Ok(db.priv_impl_alias_semantic_data(impl_alias_id, false)?.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::impl_alias_attributes].
#[salsa::tracked]
pub fn impl_alias_attributes_tracked<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
) -> Maybe<Vec<Attribute<'db>>> {
    impl_alias_attributes(db, impl_alias_id)
}

/// Implementation of [crate::db::SemanticGroup::impl_alias_impl_def].
pub fn impl_alias_impl_def<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
) -> Maybe<ImplDefId<'db>> {
    let module_file_id = impl_alias_id.module_file_id(db);
    let mut diagnostics = SemanticDiagnostics::default();
    let impl_alias_ast = db.module_impl_alias_by_id(impl_alias_id)?;
    let inference_id = InferenceId::ImplAliasImplDef(impl_alias_id);

    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    resolver.set_feature_config(&impl_alias_id, &impl_alias_ast, &mut diagnostics);

    let impl_path_syntax = impl_alias_ast.impl_path(db);

    match resolver.resolve_generic_path_with_args(
        &mut diagnostics,
        &impl_path_syntax,
        NotFoundItemType::Impl,
        ResolutionContext::Default,
    ) {
        Ok(ResolvedGenericItem::Impl(imp)) => Ok(imp),
        Ok(ResolvedGenericItem::GenericImplAlias(impl_alias)) => db.impl_alias_impl_def(impl_alias),
        // Skipping diagnostics since we will get these through when resolving in the
        // `priv_impl_alias_semantic_data` query.
        _ => Err(skip_diagnostic()),
    }
}

/// Query implementation of [crate::db::SemanticGroup::impl_alias_impl_def].
#[salsa::tracked(cycle_result=impl_alias_impl_def_cycle)]
pub fn impl_alias_impl_def_tracked<'db>(
    db: &'db dyn SemanticGroup,
    impl_alias_id: ImplAliasId<'db>,
) -> Maybe<ImplDefId<'db>> {
    impl_alias_impl_def(db, impl_alias_id)
}

/// Cycle handling for [crate::db::SemanticGroup::impl_alias_impl_def].
pub fn impl_alias_impl_def_cycle<'db>(
    _db: &dyn SemanticGroup,
    _impl_alias_id: ImplAliasId<'db>,
) -> Maybe<ImplDefId<'db>> {
    // Skipping diagnostics since we will get these through when resolving in the
    // `priv_impl_alias_semantic_data` query.
    Err(skip_diagnostic())
}
