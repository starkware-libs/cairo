use cairo_lang_defs::ids::TraitFunctionId;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::inference::infers::InferenceEmbeddings;
use cairo_lang_semantic::expr::inference::solver::SolutionSet;
use cairo_lang_semantic::expr::inference::{ImplVarTraitItemMappings, InferenceId};
use cairo_lang_semantic::lsp_helpers::TypeFilter;
use cairo_lang_semantic::resolve::Resolver;
use tracing::debug;

use crate::lang::db::AnalysisDatabase;

/// Finds all methods that can be called on a type.
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
                inference.trait_solution_set(
                    concrete_trait_id,
                    ImplVarTraitItemMappings::default(),
                    lookup_context
                ),
                Ok(SolutionSet::Unique(_) | SolutionSet::Ambiguous(_))
            ) {
                continue;
            }
            relevant_methods.push(trait_function);
        }
    }
    relevant_methods
}
