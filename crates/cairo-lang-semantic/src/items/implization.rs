use cairo_lang_defs::ids::{ImplDefId, TraitTypeId};
use cairo_lang_diagnostics::Maybe;
use salsa::Database;

use crate::TypeId;
use crate::items::imp::ImplSemantic;

/// Implementation of [ImplizationSemantic::trait_type_implized_by_context].
fn trait_type_implized_by_context<'db>(
    db: &'db dyn Database,
    trait_type_id: TraitTypeId<'db>,
    impl_def_id: ImplDefId<'db>,
) -> Maybe<TypeId<'db>> {
    let impl_type_def_id = db.impl_type_by_trait_type(impl_def_id, trait_type_id)?;

    db.impl_type_def_resolved_type(impl_type_def_id)
}

/// Query implementation of [ImplizationSemantic::trait_type_implized_by_context].
#[salsa::tracked(cycle_result=trait_type_implized_by_context_cycle)]
fn trait_type_implized_by_context_tracked<'db>(
    db: &'db dyn Database,
    trait_type_id: TraitTypeId<'db>,
    impl_def_id: ImplDefId<'db>,
) -> Maybe<TypeId<'db>> {
    trait_type_implized_by_context(db, trait_type_id, impl_def_id)
}

/// Cycle handling for [ImplizationSemantic::trait_type_implized_by_context].
fn trait_type_implized_by_context_cycle<'db>(
    db: &'db dyn Database,
    trait_type_id: TraitTypeId<'db>,
    impl_def_id: ImplDefId<'db>,
) -> Maybe<TypeId<'db>> {
    // Forwarding cycle handling to `priv_impl_type_semantic_data` handler.
    trait_type_implized_by_context(db, trait_type_id, impl_def_id)
}

/// Trait for implization-related semantic queries.
pub trait ImplizationSemantic<'db>: Database {
    /// Returns the impl type for the given trait type, by implization by the given impl context, if
    /// the impl matches the trait of the trait type.
    // TODO(Gil): Consider removing the cycle handling here if we will upgrade the salsa version.
    fn trait_type_implized_by_context(
        &'db self,
        trait_type_def_id: TraitTypeId<'db>,
        impl_def_id: ImplDefId<'db>,
    ) -> Maybe<TypeId<'db>> {
        trait_type_implized_by_context_tracked(
            self.as_dyn_database(),
            trait_type_def_id,
            impl_def_id,
        )
    }
}
impl<'db, T: Database + ?Sized> ImplizationSemantic<'db> for T {}
