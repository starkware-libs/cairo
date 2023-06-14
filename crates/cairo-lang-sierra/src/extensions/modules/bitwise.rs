use crate::extensions::NoGenericArgsGenericType;
use crate::ids::GenericTypeId;

/// Type representing the Bitwise builtin.
#[derive(Default)]
pub struct BitwiseType {}
impl NoGenericArgsGenericType for BitwiseType {
    const ID: GenericTypeId = GenericTypeId::new_inline("Bitwise");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const ZERO_SIZED: bool = false;
}
