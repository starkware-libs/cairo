use std::sync::Arc;

use cairo_lang_defs::ids::{LanguageElementId, LookupItemId, ModuleItemId, TraitAliasId, TraitId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_utils::try_extract_matches;

use super::generics::{semantic_generic_params, GenericParamsData};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics};
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::InferenceId;
use crate::resolve::{ResolvedConcreteItem, ResolvedGenericItem, Resolver, ResolverData};
use crate::substitution::SemanticRewriter;
use crate::{ConcreteTraitId, GenericParam, SemanticDiagnostic};

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct TraitAliasData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    resolved_trait: Maybe<ConcreteTraitId>,
    generic_params: Vec<GenericParam>,
    attributes: Vec<Attribute>,
    resolver_data: Arc<ResolverData>,
}
impl TraitAliasData {
    /// Returns Maybe::Err if a cycle is detected here.
    // TODO(orizi): Remove this function when cycle validation is not required through a type's
    // field.
    pub fn check_no_cycle(&self) -> Maybe<()> {
        self.resolved_trait?;
        Ok(())
    }
}

/// Query implementation of [crate::db::SemanticGroup::priv_trait_alias_semantic_data].
pub fn priv_trait_alias_semantic_data(
    db: &(dyn SemanticGroup),
    trait_alias_id: TraitAliasId,
) -> Maybe<TraitAliasData> {
    let module_file_id = trait_alias_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let module_trait_aliases = db.module_trait_aliases(module_file_id.0)?;
    let trait_alias_ast = module_trait_aliases.get(&trait_alias_id).to_maybe()?;
    let syntax_db = db.upcast();
    let generic_params_data = db.trait_alias_generic_params_data(trait_alias_id)?;
    let generic_params = generic_params_data.generic_params.clone();
    let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(
        ModuleItemId::TraitAlias(trait_alias_id),
    ));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.diagnostics.extend(generic_params_data.diagnostics);

    let item = resolver.resolve_concrete_path(
        &mut diagnostics,
        &trait_alias_ast.trait_path(syntax_db),
        NotFoundItemType::Trait,
    );
    let resolved_trait = item.and_then(|item| {
        try_extract_matches!(item, ResolvedConcreteItem::Trait)
            .ok_or_else(|| diagnostics.report(&trait_alias_ast.trait_path(syntax_db), UnknownTrait))
    });

    // Check fully resolved.
    if let Some((stable_ptr, inference_err)) = resolver.inference().finalize() {
        inference_err
            .report(&mut diagnostics, stable_ptr.unwrap_or(trait_alias_ast.stable_ptr().untyped()));
    }
    let resolved_trait = resolver.inference().rewrite(resolved_trait).no_err();
    let generic_params = resolver.inference().rewrite(generic_params).no_err();

    let attributes = trait_alias_ast.attributes(syntax_db).structurize(syntax_db);
    let resolver_data = Arc::new(resolver.data);
    Ok(TraitAliasData {
        diagnostics: diagnostics.build(),
        resolved_trait,
        generic_params,
        attributes,
        resolver_data,
    })
}

/// Cycle handling for [crate::db::SemanticGroup::priv_trait_alias_semantic_data].
pub fn priv_trait_alias_semantic_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    trait_alias_id: &TraitAliasId,
) -> Maybe<TraitAliasData> {
    let module_file_id = trait_alias_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let module_trait_aliases = db.module_trait_aliases(module_file_id.0)?;
    let trait_alias_ast = module_trait_aliases.get(trait_alias_id).to_maybe()?;
    let syntax_db = db.upcast();
    let err = Err(diagnostics.report(&trait_alias_ast.name(syntax_db), TraitAliasCycle));
    let generic_params_data = db.trait_alias_generic_params_data(*trait_alias_id)?;
    let generic_params = generic_params_data.generic_params.clone();
    diagnostics.diagnostics.extend(generic_params_data.diagnostics);
    let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(
        ModuleItemId::TraitAlias(*trait_alias_id),
    ));
    let attributes = trait_alias_ast.attributes(syntax_db).structurize(syntax_db);

    Ok(TraitAliasData {
        diagnostics: diagnostics.build(),
        resolved_trait: err,
        generic_params,
        attributes,
        resolver_data: Arc::new(ResolverData::new(module_file_id, inference_id)),
    })
}

/// Query implementation of [crate::db::SemanticGroup::trait_alias_semantic_diagnostics].
pub fn trait_alias_semantic_diagnostics(
    db: &dyn SemanticGroup,
    trait_alias_id: TraitAliasId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_trait_alias_semantic_data(trait_alias_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::trait_alias_resolved_trait].
pub fn trait_alias_resolved_trait(
    db: &dyn SemanticGroup,
    trait_alias_id: TraitAliasId,
) -> Maybe<ConcreteTraitId> {
    db.priv_trait_alias_semantic_data(trait_alias_id)?.resolved_trait
}

/// Query implementation of [crate::db::SemanticGroup::trait_alias_generic_params].
pub fn trait_alias_generic_params(
    db: &dyn SemanticGroup,
    trait_alias_id: TraitAliasId,
) -> Maybe<Vec<GenericParam>> {
    Ok(db.trait_alias_generic_params_data(trait_alias_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::trait_alias_generic_params_data].
pub fn trait_alias_generic_params_data(
    db: &dyn SemanticGroup,
    trait_alias_id: TraitAliasId,
) -> Maybe<GenericParamsData> {
    let module_file_id = trait_alias_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let module_trait_aliases = db.module_trait_aliases(module_file_id.0)?;
    let trait_alias_ast = module_trait_aliases.get(&trait_alias_id).to_maybe()?;
    let syntax_db = db.upcast();
    let inference_id = InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(
        ModuleItemId::TraitAlias(trait_alias_id),
    ));
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &trait_alias_ast.generic_params(syntax_db),
    )?;
    resolver.inference().finalize().map(|(_, inference_err)| {
        inference_err.report(&mut diagnostics, trait_alias_ast.stable_ptr().untyped())
    });
    let generic_params = resolver.inference().rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { diagnostics: diagnostics.build(), generic_params, resolver_data })
}

/// Query implementation of [crate::db::SemanticGroup::trait_alias_resolver_data].
pub fn trait_alias_resolver_data(
    db: &dyn SemanticGroup,
    trait_alias_id: TraitAliasId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_trait_alias_semantic_data(trait_alias_id)?.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::trait_alias_attributes].
pub fn trait_alias_attributes(
    db: &dyn SemanticGroup,
    trait_alias_id: TraitAliasId,
) -> Maybe<Vec<Attribute>> {
    Ok(db.priv_trait_alias_semantic_data(trait_alias_id)?.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::trait_alias_trait].
pub fn trait_alias_trait(db: &dyn SemanticGroup, trait_alias_id: TraitAliasId) -> Maybe<TraitId> {
    let module_file_id = trait_alias_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);

    let module_trait_aliases = db.module_trait_aliases(module_file_id.0)?;
    let syntax_db = db.upcast();
    let trait_alias_ast = module_trait_aliases.get(&trait_alias_id).to_maybe()?;
    let inference_id = InferenceId::TraitAliasTrait(trait_alias_id);

    let mut resolver = Resolver::new(db, module_file_id, inference_id);

    let trait_path_syntax = trait_alias_ast.trait_path(syntax_db);

    resolver
        .resolve_generic_path_with_args(
            &mut diagnostics,
            &trait_path_syntax,
            NotFoundItemType::Trait,
        )
        .ok()
        .and_then(|generic_item| try_extract_matches!(generic_item, ResolvedGenericItem::Trait))
        .ok_or_else(|| diagnostics.report(&trait_path_syntax, NotATrait))
}
