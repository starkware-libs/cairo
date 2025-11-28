use cairo_lang_lowering::ids::LocationId;
use cairo_lang_sierra::ids::VarId;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

/// The debug info of a sierra function.
/// Contains a signature location and locations of sierra variables of this function.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionDebugInfo<'db> {
    pub signature_location: LocationId<'db>,
    pub variables_locations: OrderedHashMap<VarId, LocationId<'db>>,
}
