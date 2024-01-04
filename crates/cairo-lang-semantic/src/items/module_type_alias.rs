use std::sync::Arc;

use cairo_lang_defs::ids::{LanguageElementId, LookupItemId, ModuleItemId, ModuleTypeAliasId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::node::TypedSyntaxNode;

use super::generics::{semantic_generic_params, GenericParamsData};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::InferenceId;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::SemanticRewriter;
use crate::types::resolve_type;
use crate::{GenericParam, SemanticDiagnostic, TypeId};

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct TypeAliasData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    resolved_type: Maybe<TypeId>,
    generic_params: Vec<GenericParam>,
    resolver_data: Arc<ResolverData>,
}
impl TypeAliasData {
    /// Returns Maybe::Err if a cycle is detected here.
    // TODO(orizi): Remove this function when cycle validation is not required through a type's
    // field.
    pub fn check_no_cycle(&self) -> Maybe<()> {
        self.resolved_type?;
        Ok(())
    }
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::module_type_alias_semantic_diagnostics].
pub fn module_type_alias_semantic_diagnostics(
    db: &dyn SemanticGroup,
    module_type_alias_id: ModuleTypeAliasId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_module_type_alias_semantic_data(module_type_alias_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::module_type_alias_resolved_type].
pub fn module_type_alias_resolved_type(
    db: &dyn SemanticGroup,
    module_type_alias_id: ModuleTypeAliasId,
) -> Maybe<TypeId> {
    db.priv_module_type_alias_semantic_data(module_type_alias_id)?.resolved_type
}

/// Query implementation of [crate::db::SemanticGroup::module_type_alias_generic_params].
pub fn module_type_alias_generic_params(
    db: &dyn SemanticGroup,
    module_type_alias_id: ModuleTypeAliasId,
) -> Maybe<Vec<GenericParam>> {
    Ok(db.priv_module_type_alias_generic_params_data(module_type_alias_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::module_type_alias_resolver_data].
pub fn module_type_alias_resolver_data(
    db: &dyn SemanticGroup,
    module_type_alias_id: ModuleTypeAliasId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_module_type_alias_semantic_data(module_type_alias_id)?.resolver_data)
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_module_type_alias_generic_params_data].
pub fn priv_module_type_alias_generic_params_data(
    db: &dyn SemanticGroup,
    module_type_alias_id: ModuleTypeAliasId,
) -> Maybe<GenericParamsData> {
    let module_file_id = module_type_alias_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let module_type_alias_ast = db.module_type_alias_by_id(module_type_alias_id)?.to_maybe()?;
    let inference_id = InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(
        ModuleItemId::TypeAlias(module_type_alias_id),
    ));
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &module_type_alias_ast.generic_params(db.upcast()),
    )?;

    resolver.inference().finalize().map(|(_, inference_err)| {
        inference_err.report(&mut diagnostics, module_type_alias_ast.stable_ptr().untyped())
    });
    let generic_params = resolver.inference().rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { diagnostics: diagnostics.build(), generic_params, resolver_data })
}

/// Query implementation of [crate::db::SemanticGroup::priv_module_type_alias_semantic_data].
pub fn priv_module_type_alias_semantic_data(
    db: &(dyn SemanticGroup),
    module_type_alias_id: ModuleTypeAliasId,
) -> Maybe<TypeAliasData> {
    let module_file_id = module_type_alias_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let module_type_aliases = db.module_type_aliases(module_file_id.0)?;
    let module_type_alias_ast = module_type_aliases.get(&module_type_alias_id).to_maybe()?;
    let syntax_db = db.upcast();
    let generic_params_data =
        db.priv_module_type_alias_generic_params_data(module_type_alias_id)?;
    let generic_params = generic_params_data.generic_params;
    let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(
        ModuleItemId::TypeAlias(module_type_alias_id),
    ));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.diagnostics.extend(generic_params_data.diagnostics);

    let ty =
        resolve_type(db, &mut diagnostics, &mut resolver, &module_type_alias_ast.ty(syntax_db));

    // Check fully resolved.
    if let Some((stable_ptr, inference_err)) = resolver.inference().finalize() {
        inference_err.report(
            &mut diagnostics,
            stable_ptr.unwrap_or(module_type_alias_ast.stable_ptr().untyped()),
        );
    }
    let generic_params = resolver.inference().rewrite(generic_params).no_err();
    let ty = resolver.inference().rewrite(ty).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(TypeAliasData {
        diagnostics: diagnostics.build(),
        resolved_type: Ok(ty),
        generic_params,
        resolver_data,
    })
}

/// Cycle handling for [crate::db::SemanticGroup::priv_module_type_alias_semantic_data].
pub fn priv_module_type_alias_semantic_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    module_type_alias_id: &ModuleTypeAliasId,
) -> Maybe<TypeAliasData> {
    let module_file_id = module_type_alias_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let module_type_aliases = db.module_type_aliases(module_file_id.0)?;
    let module_type_alias_ast = module_type_aliases.get(module_type_alias_id).to_maybe()?;
    let syntax_db = db.upcast();
    let err = Err(diagnostics.report(&module_type_alias_ast.name(syntax_db), TypeAliasCycle));
    let generic_params_data =
        db.priv_module_type_alias_generic_params_data(*module_type_alias_id)?;
    let generic_params = generic_params_data.generic_params;
    let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(
        ModuleItemId::TypeAlias(*module_type_alias_id),
    ));
    let resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.diagnostics.extend(generic_params_data.diagnostics);

    Ok(TypeAliasData {
        diagnostics: diagnostics.build(),
        resolved_type: err,
        generic_params,
        resolver_data: Arc::new(resolver.data),
    })
}
