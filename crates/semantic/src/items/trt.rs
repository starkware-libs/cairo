use db_utils::define_short_id;
use defs::ids::{GenericParamId, ImplId, LanguageElementId, ModuleId, TraitId};
use diagnostics::Diagnostics;
use diagnostics_proc_macros::DebugWithDb;
use utils::{try_extract_matches, OptionHelper};

use super::generics::semantic_generic_params;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::resolve_path::{ResolvedConcreteItem, Resolver};
use crate::{GenericArgumentId, SemanticDiagnostic};

#[cfg(test)]
#[path = "trt_test.rs"]
mod test;

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ConcreteTraitLongId {
    pub trait_id: TraitId,
    pub generic_args: Vec<GenericArgumentId>,
}
define_short_id!(ConcreteTraitId, ConcreteTraitLongId, SemanticGroup, lookup_intern_concrete_trait);

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct TraitData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    generic_params: Vec<GenericParamId>,
}

/// Query implementation of [crate::db::SemanticGroup::trait_semantic_diagnostics].
pub fn trait_semantic_diagnostics(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_trait_semantic_data(trait_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::trait_generic_params].
pub fn trait_generic_params(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Option<Vec<GenericParamId>> {
    Some(db.priv_trait_semantic_data(trait_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::priv_trait_semantic_data].
pub fn priv_trait_semantic_data(db: &dyn SemanticGroup, trait_id: TraitId) -> Option<TraitData> {
    // TODO(spapini): When asts are rooted on items, don't query module_data directly. Use a
    // selector.
    let module_id = trait_id.module(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_id);
    let module_data = db.module_data(module_id)?;
    let trait_ast = module_data.traits.get(&trait_id)?;

    // Generic params.
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        module_id,
        &trait_ast.generic_params(db.upcast()),
    );

    Some(TraitData { diagnostics: diagnostics.build(), generic_params })
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ConcreteImplLongId {
    pub impl_id: ImplId,
    pub generic_args: Vec<GenericArgumentId>,
}
define_short_id!(ConcreteImplId, ConcreteImplLongId, SemanticGroup, lookup_intern_concrete_impl);

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    generic_params: Vec<GenericParamId>,
    /// The concrete trait this impl implements, or None if cannot be resolved.
    concrete_trait: Option<ConcreteTraitId>,
}

/// Query implementation of [crate::db::SemanticGroup::impl_semantic_diagnostics].
pub fn impl_semantic_diagnostics(
    db: &dyn SemanticGroup,
    impl_id: ImplId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_impl_semantic_data(impl_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::impl_generic_params].
pub fn impl_generic_params(db: &dyn SemanticGroup, impl_id: ImplId) -> Option<Vec<GenericParamId>> {
    Some(db.priv_impl_semantic_data(impl_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::priv_impl_semantic_data].
pub fn priv_impl_semantic_data(db: &dyn SemanticGroup, impl_id: ImplId) -> Option<ImplData> {
    // TODO(spapini): When asts are rooted on items, don't query module_data directly. Use a
    // selector.
    let module_id = impl_id.module(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_id);
    // TODO(spapini): Add generic args when they are supported on enums.
    let module_data = db.module_data(module_id)?;
    let impl_ast = module_data.impls.get(&impl_id)?;
    let syntax_db = db.upcast();

    // Generic params.
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        module_id,
        &impl_ast.generic_params(syntax_db),
    );
    let mut resolver = Resolver::new(db, module_id, &generic_params);

    let trait_path_syntax = impl_ast.trait_path(syntax_db);
    let concrete_trait = resolver
        .resolve_concrete_path(&mut diagnostics, &trait_path_syntax)
        .and_then(|option_concrete_path| {
            try_extract_matches!(option_concrete_path, ResolvedConcreteItem::Trait)
                .on_none(|| diagnostics.report(&trait_path_syntax, NotATrait))
        });

    Some(ImplData { diagnostics: diagnostics.build(), generic_params, concrete_trait })
}

/// Query implementation of [crate::db::SemanticGroup::find_impls_at_module].
pub fn find_impls_at_module(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    concrete_trait_id: ConcreteTraitId,
) -> Option<Vec<ConcreteImplId>> {
    let mut res = Vec::new();
    let impls = db.module_data(module_id)?.impls;
    // TODO(spapini): Index better.
    for impl_id in impls.keys().copied() {
        let Some(imp_data)= db.priv_impl_semantic_data(impl_id) else {continue};
        if !imp_data.generic_params.is_empty() {
            // TODO(spapini): Infer generics and substitute.
            continue;
        }

        if imp_data.concrete_trait == Some(concrete_trait_id) {
            let concrete_impl_id =
                db.intern_concrete_impl(ConcreteImplLongId { impl_id, generic_args: vec![] });
            res.push(concrete_impl_id);
        }
    }
    Some(res)
}
