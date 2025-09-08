use cairo_lang_defs::ids::{ImplDefId, TraitTypeId};
use cairo_lang_diagnostics::Maybe;
use salsa::Database;

use crate::TypeId;
use crate::db::SemanticGroup;

/// Implementation of [crate::db::SemanticGroup::trait_type_implized_by_context].
pub fn trait_type_implized_by_context<'db>(
    db: &'db dyn Database,
    trait_type_id: TraitTypeId<'db>,
    impl_def_id: ImplDefId<'db>,
) -> Maybe<TypeId<'db>> {
    let impl_type_def_id = db.impl_type_by_trait_type(impl_def_id, trait_type_id)?;

    db.impl_type_def_resolved_type(impl_type_def_id)
}

/// Query implementation of [crate::db::SemanticGroup::trait_type_implized_by_context].
#[salsa::tracked(cycle_result=trait_type_implized_by_context_cycle)]
pub fn trait_type_implized_by_context_tracked<'db>(
    db: &'db dyn Database,
    trait_type_id: TraitTypeId<'db>,
    impl_def_id: ImplDefId<'db>,
) -> Maybe<TypeId<'db>> {
    trait_type_implized_by_context(db, trait_type_id, impl_def_id)
}

/// Cycle handling for [crate::db::SemanticGroup::trait_type_implized_by_context].
pub fn trait_type_implized_by_context_cycle<'db>(
    db: &'db dyn Database,
    trait_type_id: TraitTypeId<'db>,
    impl_def_id: ImplDefId<'db>,
) -> Maybe<TypeId<'db>> {
    // Forwarding cycle handling to `priv_impl_type_semantic_data` handler.
    trait_type_implized_by_context(db, trait_type_id, impl_def_id)
}
