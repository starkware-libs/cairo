use crate::extensions::NoGenericArgsGenericType;
use crate::ids::GenericTypeId;

/// Type for StarkNet system object.
/// Used to make system calls.
#[derive(Default)]
pub struct SystemType {}
impl NoGenericArgsGenericType for SystemType {
    const ID: GenericTypeId = GenericTypeId::new_inline("System");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const SIZE: i16 = 1;
}
