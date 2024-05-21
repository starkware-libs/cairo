use crate::extensions::NoGenericArgsGenericType;
use crate::ids::GenericTypeId;

/// Type for Range Check builtin.
#[derive(Default, Debug)]
pub struct RangeCheckType {}
impl NoGenericArgsGenericType for RangeCheckType {
    const ID: GenericTypeId = GenericTypeId::new_inline("RangeCheck");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const ZERO_SIZED: bool = false;
}

/// Type for Range Check builtin.
#[derive(Default, Debug)]
pub struct RangeCheck96Type {}
impl NoGenericArgsGenericType for RangeCheck96Type {
    const ID: GenericTypeId = GenericTypeId::new_inline("RangeCheck96");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const ZERO_SIZED: bool = false;
}
