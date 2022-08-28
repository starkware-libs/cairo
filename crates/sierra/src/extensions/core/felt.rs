use crate::extensions::{ConcreteType, NoGenericArgsGenericType};
use crate::ids::GenericTypeId;

/// Type for felt.
/// The native type of the Cairo architecture.
#[derive(Default)]
pub struct FeltType {}
impl NoGenericArgsGenericType for FeltType {
    type Concrete = FeltConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("felt");
}
#[derive(Default)]
pub struct FeltConcreteType {}
impl ConcreteType for FeltConcreteType {}
