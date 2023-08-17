//! Additional information about Sierra required for generating it correctly, that is not
//! extractable from [cairo_lang_sierra].

use cairo_lang_sierra::extensions::enm::EnumType;
use cairo_lang_sierra::extensions::non_zero::NonZeroType;
use cairo_lang_sierra::extensions::snapshot::SnapshotType;
use cairo_lang_sierra::extensions::structure::StructType;
use cairo_lang_sierra::extensions::uninitialized::UninitializedType;
use cairo_lang_sierra::extensions::NamedType;
use cairo_lang_sierra::ids::GenericTypeId;

/// Returns `true` if the given generic type has a constant size in memory, independently of its
/// args.
pub fn type_has_const_size(generic_type_id: &GenericTypeId) -> bool {
    ![StructType::ID, EnumType::ID, SnapshotType::ID, UninitializedType::ID, NonZeroType::ID]
        .contains(generic_type_id)
}
