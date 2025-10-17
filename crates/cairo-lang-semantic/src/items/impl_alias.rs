use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    ImplAliasId, ImplDefId, LanguageElementId, LookupItemId, ModuleId, ModuleItemId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, MaybeAsRef, skip_diagnostic};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::try_extract_matches;
use salsa::Database;

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
#[debug_db(dyn Database)]
pub struct ImplAliasData<'db> {
    pub diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    pub resolved_impl: Maybe<ImplId<'db>>,
    attributes: Vec<Attribute<'db>>,
    pub resolver_data: Arc<ResolverData<'db>>,
}

/// Returns data about a type alias.
#[salsa::tracked(cycle_result=impl_alias_semantic_data_cycle, returns(ref))]
fn impl_alias_semantic_data<'db>(
    db: &'db dyn Database,
    impl_alias_id: ImplAliasId<'db>,
    in_cycle: bool,
) -> Maybe<ImplAliasData<'db>> {
    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::ImplAlias(impl_alias_id));
    let impl_alias_ast = db.module_impl_alias_by_id(impl_alias_id)?;

    let generic_params_data =
        impl_alias_generic_params_data(db, impl_alias_id).maybe_as_ref()?.clone();

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

/// A helper function to compute the semantic data of an impl-alias item.
pub fn impl_alias_semantic_data_helper<'db>(
    db: &'db dyn Database,
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

    let attributes = impl_alias_ast.attributes(db).structurize(db);
    let resolver_data = Arc::new(resolver.data);
    Ok(ImplAliasData { diagnostics: diagnostics.build(), resolved_impl, attributes, resolver_data })
}

fn impl_alias_semantic_data_cycle<'db>(
    db: &'db dyn Database,
    impl_alias_id: ImplAliasId<'db>,
    _in_cycle: bool,
) -> Maybe<ImplAliasData<'db>> {
    impl_alias_semantic_data(db, impl_alias_id, true).clone()
}

/// A helper function to compute the semantic data of an impl-alias item when a cycle is detected.
pub fn impl_alias_semantic_data_cycle_helper<'db>(
    db: &'db dyn Database,
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
    diagnostics.extend(generic_params_data.diagnostics);
    let inference_id = InferenceId::LookupItemDeclaration(lookup_item_id);
    let attributes = impl_alias_ast.attributes(db).structurize(db);
    Ok(ImplAliasData {
        diagnostics: diagnostics.build(),
        resolved_impl: err,
        attributes,
        resolver_data: (*generic_params_data.resolver_data)
            .clone_with_inference_id(db, inference_id)
            .into(),
    })
}

/// Returns the generic parameters data of a type alias.
#[salsa::tracked(returns(ref))]
fn impl_alias_generic_params_data<'db>(
    db: &'db dyn Database,
    impl_alias_id: ImplAliasId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    let module_id = impl_alias_id.module_id(db);
    let impl_alias_ast = db.module_impl_alias_by_id(impl_alias_id)?;
    impl_alias_generic_params_data_helper(
        db,
        module_id,
        &impl_alias_ast,
        LookupItemId::ModuleItem(ModuleItemId::ImplAlias(impl_alias_id)),
        None,
    )
}

/// Computes data about the generic parameters of an impl-alias item.
pub fn impl_alias_generic_params_data_helper<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
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
        None => Resolver::new(db, module_id, inference_id),
    };
    resolver.set_feature_config(&lookup_item_id, impl_alias_ast, &mut diagnostics);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_id,
        &impl_alias_ast.generic_params(db),
    );

    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, impl_alias_ast.stable_ptr(db).untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { diagnostics: diagnostics.build(), generic_params, resolver_data })
}

/// Implementation of [ImplAliasSemantic::impl_alias_impl_def].
#[salsa::tracked(cycle_result=impl_alias_impl_def_cycle)]
fn impl_alias_impl_def<'db>(
    db: &'db dyn Database,
    impl_alias_id: ImplAliasId<'db>,
) -> Maybe<ImplDefId<'db>> {
    let module_id = impl_alias_id.module_id(db);
    let mut diagnostics = SemanticDiagnostics::default();
    let impl_alias_ast = db.module_impl_alias_by_id(impl_alias_id)?;
    let inference_id = InferenceId::ImplAliasImplDef(impl_alias_id);

    let mut resolver = Resolver::new(db, module_id, inference_id);
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
        // `impl_alias_semantic_data` query.
        _ => Err(skip_diagnostic()),
    }
}

/// Cycle handling for [ImplAliasSemantic::impl_alias_impl_def].
fn impl_alias_impl_def_cycle<'db>(
    _db: &dyn Database,
    _impl_alias_id: ImplAliasId<'db>,
) -> Maybe<ImplDefId<'db>> {
    // Skipping diagnostics since we will get these through when resolving in the
    // `impl_alias_semantic_data` query.
    Err(skip_diagnostic())
}

/// Trait for impl-alias-related semantic queries.
pub trait ImplAliasSemantic<'db>: Database {
    /// Returns the impl definition pointed to by the impl alias, or an error if it points to
    /// something else.
    fn impl_alias_impl_def(&'db self, id: ImplAliasId<'db>) -> Maybe<ImplDefId<'db>> {
        impl_alias_impl_def(self.as_dyn_database(), id)
    }
    /// Returns the semantic diagnostics of a type alias.
    fn impl_alias_semantic_diagnostics(
        &'db self,
        id: ImplAliasId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        impl_alias_semantic_data(self.as_dyn_database(), id, false)
            .as_ref()
            .map(|data| data.diagnostics.clone())
            .unwrap_or_default()
    }
    /// Returns the resolved type of a type alias.
    fn impl_alias_resolved_impl(&'db self, id: ImplAliasId<'db>) -> Maybe<ImplId<'db>> {
        let db = self.as_dyn_database();
        if let Some(data) = db.cached_crate_semantic_data(id.module_id(db).owning_crate(db)) {
            if let Some(resolved_impl) = data.impl_aliases_resolved_impls.get(&id) {
                return Ok(*resolved_impl);
            } else {
                panic!(
                    "impl alias not found in cached impl_aliases_resolved_impls {:?}",
                    id.debug(db)
                );
            }
        };
        impl_alias_semantic_data(self.as_dyn_database(), id, false).maybe_as_ref()?.resolved_impl
    }
    /// Returns the generic parameters of a type alias.
    fn impl_alias_generic_params(&'db self, id: ImplAliasId<'db>) -> Maybe<Vec<GenericParam<'db>>> {
        Ok(impl_alias_generic_params_data(self.as_dyn_database(), id)
            .maybe_as_ref()?
            .generic_params
            .clone())
    }
    /// Returns the resolution resolved_items of a type alias.
    fn impl_alias_resolver_data(&'db self, id: ImplAliasId<'db>) -> Maybe<Arc<ResolverData<'db>>> {
        Ok(impl_alias_semantic_data(self.as_dyn_database(), id, false)
            .maybe_as_ref()?
            .resolver_data
            .clone())
    }
    /// Returns the attributes attached to the impl alias.
    fn impl_alias_attributes(&'db self, id: ImplAliasId<'db>) -> Maybe<&'db [Attribute<'db>]> {
        Ok(&impl_alias_semantic_data(self.as_dyn_database(), id, false).maybe_as_ref()?.attributes)
    }
}
impl<'db, T: Database + ?Sized> ImplAliasSemantic<'db> for T {}
