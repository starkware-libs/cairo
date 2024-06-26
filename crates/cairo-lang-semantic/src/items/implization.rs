use cairo_lang_defs::ids::{ImplDefId, TraitTypeId};
use cairo_lang_diagnostics::Maybe;

use crate::db::SemanticGroup;
use crate::TypeId;

/// Query implementation of [crate::db::SemanticGroup::trait_type_implized_by_context].
pub fn trait_type_implized_by_context(
    db: &dyn SemanticGroup,
    trait_type_id: TraitTypeId,
    impl_def_id: ImplDefId,
) -> Maybe<TypeId> {
    let impl_type_def_id = db.impl_type_by_trait_type(impl_def_id, trait_type_id)?;

    db.impl_type_def_resolved_type(impl_type_def_id)
}

/// Cycle handling for [crate::db::SemanticGroup::trait_type_implized_by_context].
pub fn trait_type_implized_by_context_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    trait_type_id: &TraitTypeId,
    impl_def_id: &ImplDefId,
) -> Maybe<TypeId> {
    // Forwarding cycle handling to `priv_impl_type_semantic_data` handler.
    trait_type_implized_by_context(db, *trait_type_id, *impl_def_id)
}
