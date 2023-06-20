use std::sync::Arc;

use cairo_lang_defs::ids::{ImplAliasId, LanguageElementId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_utils::try_extract_matches;

use super::generics::semantic_generic_params;
use super::imp::ImplId;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics};
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::resolve::{ResolvedConcreteItem, Resolver, ResolverData};
use crate::substitution::SemanticRewriter;
use crate::{GenericParam, SemanticDiagnostic};

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplAliasData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    resolved_impl: Maybe<ImplId>,
    generic_params: Vec<GenericParam>,
    resolver_data: Arc<ResolverData>,
}
impl ImplAliasData {
    /// Returns Maybe::Err if a cycle is detected here.
    // TODO(orizi): Remove this function when cycle validation is not required through a type's
    // field.
    pub fn check_no_cycle(&self) -> Maybe<()> {
        self.resolved_impl?;
        Ok(())
    }
}

/// Query implementation of [crate::db::SemanticGroup::priv_impl_alias_semantic_data].
pub fn priv_impl_alias_semantic_data(
    db: &(dyn SemanticGroup),
    impl_alias_id: ImplAliasId,
) -> Maybe<ImplAliasData> {
    let module_file_id = impl_alias_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let module_impl_aliases = db.module_impl_aliases(module_file_id.0)?;
    let impl_alias_ast = module_impl_aliases.get(&impl_alias_id).to_maybe()?;
    let syntax_db = db.upcast();
    let mut resolver = Resolver::new(db, module_file_id);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &impl_alias_ast.generic_params(syntax_db),
        false,
    )?;
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
    if let Some((stable_ptr, inference_err)) = resolver.inference().finalize() {
        inference_err
            .report(&mut diagnostics, stable_ptr.unwrap_or(impl_alias_ast.stable_ptr().untyped()));
    }
    let generic_params = resolver.inference().rewrite(generic_params).no_err();
    let resolved_impl = resolver.inference().rewrite(resolved_impl).no_err();

    let resolver_data = Arc::new(resolver.data);
    Ok(ImplAliasData {
        diagnostics: diagnostics.build(),
        resolved_impl,
        generic_params,
        resolver_data,
    })
}

/// Cycle handling for [crate::db::SemanticGroup::priv_impl_alias_semantic_data].
pub fn priv_impl_alias_semantic_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_alias_id: &ImplAliasId,
) -> Maybe<ImplAliasData> {
    let module_file_id = impl_alias_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    let module_impl_aliases = db.module_impl_aliases(module_file_id.0)?;
    let impl_alias_ast = module_impl_aliases.get(impl_alias_id).to_maybe()?;
    let syntax_db = db.upcast();
    let err = Err(diagnostics.report(&impl_alias_ast.name(syntax_db), ImplAliasCycle));
    let mut resolver = Resolver::new(db, module_file_id);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &impl_alias_ast.generic_params(syntax_db),
        false,
    )?;
    Ok(ImplAliasData {
        diagnostics: diagnostics.build(),
        resolved_impl: err,
        generic_params,
        resolver_data: Arc::new(ResolverData::new(module_file_id)),
    })
}

/// Query implementation of [crate::db::SemanticGroup::impl_alias_semantic_diagnostics].
pub fn impl_alias_semantic_diagnostics(
    db: &dyn SemanticGroup,
    impl_alias_id: ImplAliasId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_impl_alias_semantic_data(impl_alias_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::impl_alias_resolved_impl].
pub fn impl_alias_resolved_impl(
    db: &dyn SemanticGroup,
    impl_alias_id: ImplAliasId,
) -> Maybe<ImplId> {
    db.priv_impl_alias_semantic_data(impl_alias_id)?.resolved_impl
}

/// Query implementation of [crate::db::SemanticGroup::impl_alias_generic_params].
pub fn impl_alias_generic_params(
    db: &dyn SemanticGroup,
    impl_alias_id: ImplAliasId,
) -> Maybe<Vec<GenericParam>> {
    Ok(db.priv_impl_alias_semantic_data(impl_alias_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::impl_alias_resolver_data].
pub fn impl_alias_resolver_data(
    db: &dyn SemanticGroup,
    impl_alias_id: ImplAliasId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_impl_alias_semantic_data(impl_alias_id)?.resolver_data)
}
