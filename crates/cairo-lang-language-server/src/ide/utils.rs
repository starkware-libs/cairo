use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{ModuleId, TraitFunctionId};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::inference::infers::InferenceEmbeddings;
use cairo_lang_semantic::expr::inference::solver::SolutionSet;
use cairo_lang_semantic::expr::inference::InferenceId;
use cairo_lang_semantic::items::us::SemanticUseEx;
use cairo_lang_semantic::lsp_helpers::TypeFilter;
use cairo_lang_semantic::resolve::{ResolvedGenericItem, Resolver};
use tracing::debug;

use crate::lang::db::AnalysisDatabase;

/// Finds all methods that can be called on a type.
#[tracing::instrument(level = "trace", skip_all)]
pub fn find_methods_for_type(
    db: &AnalysisDatabase,
    mut resolver: Resolver<'_>,
    ty: cairo_lang_semantic::TypeId,
    stable_ptr: cairo_lang_syntax::node::ids::SyntaxStablePtrId,
) -> Vec<TraitFunctionId> {
    let type_filter = match ty.head(db) {
        Some(head) => TypeFilter::TypeHead(head),
        None => TypeFilter::NoFilter,
    };

    let mut relevant_methods = Vec::new();
    // Find methods on type.
    // TODO(spapini): Look only in current crate dependencies.
    for crate_id in db.crates() {
        let methods = db.methods_in_crate(crate_id, type_filter.clone());
        for trait_function in methods.iter().copied() {
            let clone_data =
                &mut resolver.inference().clone_with_inference_id(db, InferenceId::NoContext);
            let mut inference = clone_data.inference(db);
            let lookup_context = resolver.impl_lookup_context();
            // Check if trait function signature's first param can fit our expr type.
            let Some((concrete_trait_id, _)) = inference.infer_concrete_trait_by_self(
                trait_function,
                ty,
                &lookup_context,
                Some(stable_ptr),
                |_| {},
            ) else {
                debug!("can't fit");
                continue;
            };

            // Find impls for it.

            // ignore the result as nothing can be done with the error, if any.
            inference.solve().ok();
            if !matches!(
                inference.trait_solution_set(concrete_trait_id, lookup_context),
                Ok(SolutionSet::Unique(_) | SolutionSet::Ambiguous(_))
            ) {
                continue;
            }
            relevant_methods.push(trait_function);
        }
    }
    relevant_methods
}

/// Checks if a module has a trait in scope.
#[tracing::instrument(level = "trace", skip_all)]
pub fn module_has_trait(
    db: &AnalysisDatabase,
    module_id: ModuleId,
    trait_id: cairo_lang_defs::ids::TraitId,
) -> Option<bool> {
    if db.module_traits_ids(module_id).ok()?.contains(&trait_id) {
        return Some(true);
    }
    // TODO(Gil): Check if the trait is visible, and return the path of the visible use item.
    for use_id in db.module_uses_ids(module_id).ok()?.iter().copied() {
        if db.use_resolved_item(use_id) == Ok(ResolvedGenericItem::Trait(trait_id)) {
            return Some(true);
        }
    }
    Some(false)
}
