use std::sync::Arc;

use cairo_lang_defs::ids::{LanguageElementId, TypeAliasId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::node::TypedSyntaxNode;

use super::generics::semantic_generic_params;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::inference::canonic::ResultNoErrEx;
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

/// Query implementation of [crate::db::SemanticGroup::priv_type_alias_semantic_data].
pub fn priv_type_alias_semantic_data(
    db: &(dyn SemanticGroup),
    type_alias_id: TypeAliasId,
) -> Maybe<TypeAliasData> {
    let module_file_id = type_alias_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let module_type_aliases = db.module_type_aliases(module_file_id.0)?;
    let type_alias_ast = module_type_aliases.get(&type_alias_id).to_maybe()?;
    let syntax_db = db.upcast();
    let mut resolver = Resolver::new(db, module_file_id);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &type_alias_ast.generic_params(syntax_db),
        false,
    )?;
    let ty = resolve_type(db, &mut diagnostics, &mut resolver, &type_alias_ast.ty(syntax_db));

    // Check fully resolved.
    if let Some((stable_ptr, inference_err)) = resolver.inference().finalize() {
        inference_err
            .report(&mut diagnostics, stable_ptr.unwrap_or(type_alias_ast.stable_ptr().untyped()));
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

/// Cycle handling for [crate::db::SemanticGroup::priv_type_alias_semantic_data].
pub fn priv_type_alias_semantic_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    type_alias_id: &TypeAliasId,
) -> Maybe<TypeAliasData> {
    let module_file_id = type_alias_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    let module_type_aliases = db.module_type_aliases(module_file_id.0)?;
    let type_alias_ast = module_type_aliases.get(type_alias_id).to_maybe()?;
    let syntax_db = db.upcast();
    let err = Err(diagnostics.report(&type_alias_ast.name(syntax_db), TypeAliasCycle));
    let mut resolver = Resolver::new(db, module_file_id);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &type_alias_ast.generic_params(syntax_db),
        false,
    )?;
    Ok(TypeAliasData {
        diagnostics: diagnostics.build(),
        resolved_type: err,
        generic_params,
        resolver_data: Arc::new(ResolverData::new(module_file_id)),
    })
}

/// Query implementation of [crate::db::SemanticGroup::type_alias_semantic_diagnostics].
pub fn type_alias_semantic_diagnostics(
    db: &dyn SemanticGroup,
    type_alias_id: TypeAliasId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_type_alias_semantic_data(type_alias_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::type_alias_resolved_type].
pub fn type_alias_resolved_type(
    db: &dyn SemanticGroup,
    type_alias_id: TypeAliasId,
) -> Maybe<TypeId> {
    db.priv_type_alias_semantic_data(type_alias_id)?.resolved_type
}

/// Query implementation of [crate::db::SemanticGroup::type_alias_generic_params].
pub fn type_alias_generic_params(
    db: &dyn SemanticGroup,
    type_alias_id: TypeAliasId,
) -> Maybe<Vec<GenericParam>> {
    Ok(db.priv_type_alias_semantic_data(type_alias_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::type_alias_resolver_data].
pub fn type_alias_resolver_data(
    db: &dyn SemanticGroup,
    type_alias_id: TypeAliasId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_type_alias_semantic_data(type_alias_id)?.resolver_data)
}
