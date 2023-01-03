use crate::extensions::NoGenericArgsGenericType;
use crate::ids::GenericTypeId;

/// Type for Range Check builtin.
#[derive(Default)]
pub struct RangeCheckType {}
impl NoGenericArgsGenericType for RangeCheckType {
    const ID: GenericTypeId = GenericTypeId::new_inline("RangeCheck");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const SIZE: i16 = 1;
}
